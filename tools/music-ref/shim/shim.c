/* The headless "audio device" and engine stubs behind Chocolate Doom's OPL music
 * code (see SDL.h). The device runs at REF_RATE frames a second in buffers of
 * REF_BUFFER frames (Chocolate's GetSliceSize for that rate and its default
 * snd_maxslicetime_ms of 28), 16-bit stereo, and keeps every buffer it plays.
 *
 * Part of the Doom port in Elm-OS, GPL-2.0-or-later. */
#include <stdarg.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "SDL.h"
#include "SDL_mixer.h"
#include "stubs.h"
#include "ref.h"

/* ------------------------------------------------------------------ device */

static Mix_EffectFunc_t effect = NULL;
static void *effect_arg = NULL;

int16_t *ref_output = NULL;      /* interleaved stereo frames played so far */
size_t ref_output_frames = 0;
static size_t output_capacity = 0;

int Mix_QuerySpec(int *frequency, Uint16 *format, int *channels)
{
    *frequency = REF_RATE;
    *format = AUDIO_S16SYS;
    *channels = 2;
    return 1;
}

int Mix_OpenAudioDevice(int frequency, Uint16 format, int channels, int chunksize,
                        const char *device, int allowed_changes)
{
    (void) frequency; (void) format; (void) channels; (void) chunksize;
    (void) device; (void) allowed_changes;
    return 0;
}

int Mix_RegisterEffect(int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d, void *arg)
{
    (void) chan; (void) d;
    effect = f;
    effect_arg = arg;
    return 1;
}

void ref_device_buffer(void)
{
    static int16_t buffer[REF_BUFFER * 2];
    memset(buffer, 0, sizeof(buffer));
    if (effect != NULL)
    {
        effect(MIX_CHANNEL_POST, buffer, (int) sizeof(buffer), effect_arg);
    }
    if (ref_output_frames + REF_BUFFER > output_capacity)
    {
        output_capacity = output_capacity ? output_capacity * 2 : REF_RATE * 4;
        ref_output = realloc(ref_output, output_capacity * 2 * sizeof(int16_t));
    }
    memcpy(ref_output + ref_output_frames * 2, buffer, sizeof(buffer));
    ref_output_frames += REF_BUFFER;
}

void SDL_MixAudioFormat(Uint8 *dst, const Uint8 *src, Uint16 format, Uint32 len, int volume)
{
    int16_t *d = (int16_t *) dst;
    const int16_t *s = (const int16_t *) src;
    Uint32 i;
    (void) format; (void) volume;
    for (i = 0; i < len / 2; ++i)
    {
        int v = d[i] + s[i];
        d[i] = (int16_t) (v > 32767 ? 32767 : v < -32768 ? -32768 : v);
    }
}

SDL_mutex *SDL_CreateMutex(void) { return calloc(1, sizeof(SDL_mutex)); }
void SDL_DestroyMutex(SDL_mutex *m) { free(m); }
SDL_cond *SDL_CreateCond(void) { return calloc(1, sizeof(SDL_cond)); }
void SDL_DestroyCond(SDL_cond *c) { free(c); }

int SDL_CondWait(SDL_cond *c, SDL_mutex *m)
{
    (void) c; (void) m;
    ref_device_buffer();
    return 0;
}

/* ------------------------------------------------------------------ trace */

/* opl_sdl.c is compiled with its calls to the emulator renamed to these (see the
 * Makefile), so the register writes can be logged with the frame they land
 * before: MUSIC_REF_TRACE=file writes records of u32 frame, u16 register,
 * u8 value, little-endian. */
#include "opl3.h"

static FILE *trace = NULL;
static int trace_checked = 0;
static uint32_t frames_generated = 0;

void REF_GenerateStream(opl3_chip *chip, int16_t *sndptr, uint32_t numsamples)
{
    OPL3_GenerateStream(chip, sndptr, numsamples);
    frames_generated += numsamples;
}

void REF_WriteRegBuffered(opl3_chip *chip, uint16_t reg, uint8_t v)
{
    if (!trace_checked)
    {
        const char *path = getenv("MUSIC_REF_TRACE");
        trace_checked = 1;
        if (path != NULL) trace = fopen(path, "wb");
    }
    if (trace != NULL)
    {
        uint8_t rec[7];
        rec[0] = frames_generated & 0xff;
        rec[1] = (frames_generated >> 8) & 0xff;
        rec[2] = (frames_generated >> 16) & 0xff;
        rec[3] = (frames_generated >> 24) & 0xff;
        rec[4] = reg & 0xff;
        rec[5] = (reg >> 8) & 0xff;
        rec[6] = v;
        fwrite(rec, 1, 7, trace);
    }
    OPL3_WriteRegBuffered(chip, reg, v);
}

void ref_close_trace(void)
{
    if (trace != NULL) fclose(trace);
}

/* ------------------------------------------------------------------ WAD */

static byte *wad = NULL;
static int wad_lumps = 0;
static int wad_dir = 0;

int ref_load_wad(const char *path)
{
    FILE *f = fopen(path, "rb");
    long len;
    if (f == NULL) return 0;
    fseek(f, 0, SEEK_END);
    len = ftell(f);
    fseek(f, 0, SEEK_SET);
    wad = malloc(len);
    if (fread(wad, 1, len, f) != (size_t) len) { fclose(f); return 0; }
    fclose(f);
    memcpy(&wad_lumps, wad + 4, 4);
    memcpy(&wad_dir, wad + 8, 4);
    return 1;
}

void *ref_lump(const char *name, int *length)
{
    int i, found = -1;
    for (i = 0; i < wad_lumps; ++i)
    {
        char n[9] = {0};
        memcpy(n, wad + wad_dir + i * 16 + 8, 8);
        if (strncasecmp(n, name, 8) == 0) found = i;
    }
    if (found < 0) return NULL;
    {
        int pos, size;
        memcpy(&pos, wad + wad_dir + found * 16, 4);
        memcpy(&size, wad + wad_dir + found * 16 + 4, 4);
        if (length) *length = size;
        return wad + pos;
    }
}

void *W_CacheLumpName(const char *name, int tag)
{
    void *l = ref_lump(name, NULL);
    (void) tag;
    if (l == NULL) I_Error("W_CacheLumpName: %s not found", name);
    return l;
}

void W_ReleaseLumpName(const char *name) { (void) name; }

/* ------------------------------------------------------------------ engine */

void *Z_Malloc(int size, int tag, void *user) { (void) tag; (void) user; return malloc(size); }
void Z_Free(void *ptr) { free(ptr); }
void Z_Init(void) { }
void *I_Realloc(void *ptr, size_t size) { return realloc(ptr, size); }

void I_Error(const char *error, ...)
{
    va_list args;
    va_start(args, error);
    vfprintf(stderr, error, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(1);
}

char *M_TempFile(const char *s)
{
    const char *dir = getenv("TMPDIR");
    char *r = malloc(1024);
    snprintf(r, 1024, "%s/music-ref-%d-%s", dir ? dir : "/tmp", (int) getpid(), s);
    return r;
}

boolean M_WriteFile(const char *name, const void *source, int length)
{
    FILE *f = fopen(name, "wb");
    if (f == NULL) return false;
    fwrite(source, 1, length, f);
    fclose(f);
    return true;
}

int M_ReadFile(const char *name, byte **buffer) { (void) name; (void) buffer; return -1; }
int M_remove(const char *path) { return remove(path); }
FILE *M_fopen(const char *filename, const char *mode) { return fopen(filename, mode); }

int M_snprintf(char *buf, size_t buf_len, const char *s, ...)
{
    va_list args;
    int r;
    va_start(args, s);
    r = vsnprintf(buf, buf_len, s, args);
    va_end(args);
    return r;
}

boolean M_StringConcat(char *dest, const char *src, size_t dest_size)
{
    strncat(dest, src, dest_size - strlen(dest) - 1);
    return true;
}
