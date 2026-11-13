/* The music reference renderer's device (shim.c). */
#ifndef REF_H
#define REF_H
#include <stddef.h>
#include <stdint.h>

/* The OPL chip's own rate, so the emulator's resampler passes samples through. */
#define REF_RATE 49716
/* Chocolate's GetSliceSize(): the largest power of two within 28 ms. */
#define REF_BUFFER 1024

extern int16_t *ref_output;
extern size_t ref_output_frames;

void ref_device_buffer(void);
void ref_close_trace(void);
int ref_load_wad(const char *path);
void *ref_lump(const char *name, int *length);

#endif
