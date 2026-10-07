#include <lean/lean.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <brotli/encode.h>

typedef struct {
    BrotliEncoderState *encoder;
} compress_state;

// Live allocations made by this file and by the encoders it owns. Returns to zero
// when every stream has been finalized. The tests check this to detect leaks.
static atomic_long live_allocations = 0;

// For tests: when positive, the allocation that many calls from now fails. Zero,
// the default, disables this.
static atomic_long fail_countdown = 0;

static bool should_fail_allocation(void) {
    return atomic_load(&fail_countdown) > 0 && atomic_fetch_sub(&fail_countdown, 1) == 1;
}

static void *counting_alloc(void *opaque, size_t size) {
    (void)opaque;
    void *ptr = should_fail_allocation() ? NULL : malloc(size);
    if (ptr != NULL)
        atomic_fetch_add(&live_allocations, 1);
    return ptr;
}

static void counting_free(void *opaque, void *ptr) {
    (void)opaque;
    if (ptr != NULL)
        atomic_fetch_sub(&live_allocations, 1);
    free(ptr);
}

static lean_external_class *compress_class = NULL;
static pthread_once_t compress_class_once = PTHREAD_ONCE_INIT;

// Called by Lean when the last reference to the stream is dropped.
static void compress_finalize(void *ptr) {
    compress_state *state = ptr; // the pointer we gave to lean_alloc_external
    
    if (state->encoder != NULL)
        BrotliEncoderDestroyInstance(state->encoder);
    
    counting_free(NULL, state);
}

static void compress_foreach(void *ptr, b_lean_obj_arg fn) {
    // Nothing to do: compress_state holds no Lean objects.

    // Silence -Wunused-parameter
    (void)ptr;
    (void)fn;
}

static void compress_register_class(void) {
    compress_class = lean_register_external_class(compress_finalize, compress_foreach);
}

static lean_obj_res io_error(const char *what, const char *detail) {
    // Assume 'what' and 'detail' are not NULL. We only use this
    // function with concrete strings.

    char message[256];
    snprintf(message, sizeof(message), "%s: %s", what, detail);
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(message)));
}

/* Grow a byte array that holds `size bytes to a capacity of at least `capacity`. */
static lean_object *grow(lean_object *bytes, size_t size, size_t capacity) {
    lean_object *bigger = lean_alloc_sarray(1, 0, capacity);
    memcpy(lean_sarray_cptr(bigger), lean_sarray_cptr(bytes), size);
    lean_dec_ref(bytes);
    return bigger;
}

/* Compress `length` bytes from `input`. */
static lean_obj_res compress_run(compress_state *state, const uint8_t *input, size_t length, BrotliEncoderOperation operation) {
    BrotliEncoderState *encoder = state->encoder;
    const uint8_t *next_in = input;
    size_t avail_in = length;
    lean_object *output = NULL; // allocated when first needed
    size_t size     = 0;
    size_t capacity = 0;

    for (;;) {
        size_t avail_out = 0;
        
        if (!BrotliEncoderCompressStream(encoder, operation, &avail_in, &next_in, &avail_out, NULL, NULL)) {
            if (output) lean_dec_ref(output);
            return io_error("brotli", "stream compress failed");
        }

        size_t produced = 0;
        if (BrotliEncoderHasMoreOutput(encoder)) {
            size_t n = 0; // we will take all pending output
            const uint8_t *chunk = BrotliEncoderTakeOutput(encoder, &n);

            if (size + n > capacity) {
                size_t want    = size + n;
                size_t doubled = 2*capacity;
                capacity = doubled > want ? doubled : want;
                output = output ? grow(output, size, capacity)
                                : lean_alloc_sarray(1, 0, capacity);
            }

            memcpy(lean_sarray_cptr(output) + size, chunk, n);
            size     += n;
            produced += n;
        }

        // Done once all input consumed and call yields no more output.
        if (avail_in > 0 || produced > 0) continue;

        // User asked to finish but there is still output in the encoder.
        if (operation == BROTLI_OPERATION_FINISH && !BrotliEncoderIsFinished(encoder)) {
            if (output) lean_dec_ref(output);
            return io_error("brotli", "encoder did not finish");
        }

        break;
    }

    if (!output) output = lean_alloc_sarray(1, 0, 0);

    lean_sarray_set_size(output, size);
    return lean_io_result_mk_ok(output);
}

lean_obj_res datastar_brotli_new(uint8_t quality, uint8_t window_log, uint8_t mode) {
    pthread_once(&compress_class_once, compress_register_class);

    compress_state *state = should_fail_allocation() ? NULL : calloc(1, sizeof(compress_state));
    if (state == NULL) {
        return io_error("brotli", "out of memory");
    }
    atomic_fetch_add(&live_allocations, 1);

    state->encoder = BrotliEncoderCreateInstance(counting_alloc, counting_free, NULL);
    if (state->encoder == NULL) {
        counting_free(NULL, state);
        return io_error("brotli", "out of memory");
    }

    // These always return true; out of range values for quality and 
    // window are silently clamped later.
    BrotliEncoderSetParameter(state->encoder, BROTLI_PARAM_QUALITY, quality);
    BrotliEncoderSetParameter(state->encoder, BROTLI_PARAM_LGWIN,   window_log);
    BrotliEncoderSetParameter(state->encoder, BROTLI_PARAM_MODE,    mode);

    return lean_io_result_mk_ok(lean_alloc_external(compress_class, state));
}

lean_obj_res datastar_brotli_compress(b_lean_obj_arg state_obj, b_lean_obj_arg chunk) {
    compress_state *state = lean_get_external_data(state_obj);

    if (state->encoder == NULL)
        return io_error("brotli compress", "the stream has ended");

    return compress_run(state, lean_sarray_cptr(chunk), lean_sarray_size(chunk), BROTLI_OPERATION_FLUSH);
}

lean_obj_res datastar_brotli_finish(b_lean_obj_arg state_obj) {
    compress_state *state = lean_get_external_data(state_obj);

    if (state->encoder == NULL)
        return lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));

    lean_obj_res result = compress_run(state, NULL, 0, BROTLI_OPERATION_FINISH);

    BrotliEncoderDestroyInstance(state->encoder); 
    state->encoder = NULL;
    return result;
}

// For tests: the number of live allocations, see `live_allocations`.
lean_obj_res datastar_brotli_live_allocations(lean_obj_arg unit) {
    (void)unit;
    return lean_io_result_mk_ok(lean_usize_to_nat((size_t)atomic_load(&live_allocations)));
}

// For tests: make the `n`th allocation from now fail, or none if `n` is zero.
// Returns the previous setting, which is zero if that failure has happened.
lean_obj_res datastar_brotli_fail_allocation(size_t n) {
    long previous = atomic_exchange(&fail_countdown, (long)n);
    return lean_io_result_mk_ok(lean_usize_to_nat(previous > 0 ? (size_t)previous : 0));
}
