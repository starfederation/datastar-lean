// Test support only: not part of the library. Linked into `brotli_test`.

#include <lean/lean.h>
#include <stdlib.h>
#include <string.h>

#include <brotli/decode.h>

static lean_obj_res test_error(const char *detail) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(detail)));
}

// Decode as much of `bytes` as possible, from the start of a stream.
//
// Returns `(decoded, finished) : ByteArray × Bool`, where `finished` says whether
// the end of the Brotli stream was reached. Fails if `bytes` is not a prefix of a
// valid stream, or has bytes after the end of one.
lean_obj_res datastar_brotli_test_decode(b_lean_obj_arg bytes) {
    BrotliDecoderState *decoder = BrotliDecoderCreateInstance(NULL, NULL, NULL);
    if (decoder == NULL)
        return test_error("decode: out of memory");

    const uint8_t *next_in = lean_sarray_cptr(bytes);
    size_t avail_in = lean_sarray_size(bytes);
    size_t size = 0;
    size_t capacity = 4 * avail_in + 1024;
    uint8_t *output = malloc(capacity);
    BrotliDecoderResult result;

    for (;;) {
        if (output == NULL) {
            BrotliDecoderDestroyInstance(decoder);
            return test_error("decode: out of memory");
        }

        size_t avail_out = capacity - size;
        uint8_t *next_out = output + size;
        result = BrotliDecoderDecompressStream(decoder, &avail_in, &next_in, &avail_out, &next_out, NULL);
        size = capacity - avail_out;

        // The decoder can report that it needs input while it still holds output
        // that did not fit, so ask it directly.
        if (result != BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT && !BrotliDecoderHasMoreOutput(decoder))
            break;

        capacity *= 2;
        uint8_t *bigger = realloc(output, capacity);
        if (bigger == NULL)
            free(output);
        output = bigger;
    }

    lean_obj_res error = NULL;
    if (result == BROTLI_DECODER_RESULT_ERROR)
        error = test_error(BrotliDecoderErrorString(BrotliDecoderGetErrorCode(decoder)));
    else if (result == BROTLI_DECODER_RESULT_SUCCESS && avail_in > 0)
        error = test_error("decode: bytes after the end of the stream");

    BrotliDecoderDestroyInstance(decoder);

    if (error != NULL) {
        free(output);
        return error;
    }

    lean_object *decoded = lean_alloc_sarray(1, size, size);
    memcpy(lean_sarray_cptr(decoded), output, size);
    free(output);

    lean_object *pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, decoded);
    lean_ctor_set(pair, 1, lean_box(result == BROTLI_DECODER_RESULT_SUCCESS));
    return lean_io_result_mk_ok(pair);
}
