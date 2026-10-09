#include <lean/lean.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <brotli/encode.h>

typedef struct {
    pthread_mutex_t lock; // held by compress and finish, which frees the encoder
    BrotliEncoderState *encoder;
} compress_state;

#ifdef DATASTAR_BROTLI_TESTING

// Instrumentation for `brotli_test`, which links the bindings built with
// DATASTAR_BROTLI_TESTING: every allocation is counted, and one can be made to
// fail. The library is built without it.

#include <stdatomic.h>
#include <stdbool.h>

// Live allocations made by this file and by the encoders it owns. Returns to zero
// when every stream has been finalized. The tests check this to detect leaks.
static atomic_long live_allocations = 0;

// When positive, the allocation that many calls from now fails. Zero, the
// default, disables this.
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

// A stream's own state and everything inside its encoder are allocated alike.
static void *state_alloc(size_t size) { return counting_alloc(NULL, size); }
static void state_free(void *ptr) { counting_free(NULL, ptr); }
static const brotli_alloc_func encoder_alloc = counting_alloc;
static const brotli_free_func encoder_free = counting_free;

// The number of live allocations, see `live_allocations`.
lean_obj_res datastar_brotli_live_allocations(lean_obj_arg unit) {
    (void)unit;
    return lean_io_result_mk_ok(lean_usize_to_nat((size_t)atomic_load(&live_allocations)));
}

// Make the `n`th allocation from now fail, or none if `n` is zero.
// Returns the previous setting, which is zero if that failure has happened.
lean_obj_res datastar_brotli_fail_allocation(size_t n) {
    long previous = atomic_exchange(&fail_countdown, (long)n);
    return lean_io_result_mk_ok(lean_usize_to_nat(previous > 0 ? (size_t)previous : 0));
}

#else

static void *state_alloc(size_t size) { return malloc(size); }
static void state_free(void *ptr) { free(ptr); }
// Brotli then allocates with malloc and free.
static const brotli_alloc_func encoder_alloc = NULL;
static const brotli_free_func encoder_free = NULL;

#endif

static lean_external_class *compress_class = NULL;
static pthread_once_t compress_class_once = PTHREAD_ONCE_INIT;

// Called by Lean when the last reference to the stream is dropped.
static void compress_finalize(void *ptr) {
    compress_state *state = ptr; // the pointer we gave to lean_alloc_external
    
    if (state->encoder != NULL)
        BrotliEncoderDestroyInstance(state->encoder);

    // No reference is left, so no call holds the lock.
    pthread_mutex_destroy(&state->lock);
    state_free(state);
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

    compress_state *state = state_alloc(sizeof *state);
    if (state == NULL) {
        return io_error("brotli", "out of memory");
    }

    if (pthread_mutex_init(&state->lock, NULL) != 0) {
        state_free(state);
        return io_error("brotli", "cannot create a lock");
    }

    state->encoder = BrotliEncoderCreateInstance(encoder_alloc, encoder_free, NULL);
    if (state->encoder == NULL) {
        pthread_mutex_destroy(&state->lock);
        state_free(state);
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
    pthread_mutex_lock(&state->lock);

    lean_obj_res result = state->encoder == NULL
        ? io_error("brotli compress", "the stream has ended")
        : compress_run(state, lean_sarray_cptr(chunk), lean_sarray_size(chunk), BROTLI_OPERATION_FLUSH);

    pthread_mutex_unlock(&state->lock);
    return result;
}

lean_obj_res datastar_brotli_finish(b_lean_obj_arg state_obj) {
    compress_state *state = lean_get_external_data(state_obj);
    pthread_mutex_lock(&state->lock);

    lean_obj_res result;
    if (state->encoder == NULL) {
        result = lean_io_result_mk_ok(lean_alloc_sarray(1, 0, 0));
    } else {
        result = compress_run(state, NULL, 0, BROTLI_OPERATION_FINISH);
        BrotliEncoderDestroyInstance(state->encoder);
        state->encoder = NULL;
    }

    pthread_mutex_unlock(&state->lock);
    return result;
}
