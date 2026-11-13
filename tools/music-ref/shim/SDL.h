/* A stand-in for the parts of SDL2 that Chocolate Doom's OPL music code uses, so
 * that its own files (chocolate/) compile unchanged into a headless, deterministic
 * renderer. There are no threads: the "audio device" is ref_device_buffer() in
 * shim.c, which runs the registered mixer callback for one buffer, and waiting on
 * a condition (OPL_Delay) runs the device until the condition is signalled.
 *
 * Part of the Doom port in Elm-OS, GPL-2.0-or-later. */
#ifndef REF_SDL_H
#define REF_SDL_H

#include <stdint.h>
#include <string.h>
#include "SDL_endian.h"

typedef uint8_t Uint8;
typedef uint16_t Uint16;
typedef uint32_t Uint32;
typedef int16_t Sint16;

typedef struct SDL_mutex { int unused; } SDL_mutex;
typedef struct SDL_cond { int *flag; } SDL_cond;

#define SDL_INIT_AUDIO 0x10
#define AUDIO_S16SYS 0x8010
#define SDL_AUDIO_ALLOW_FREQUENCY_CHANGE 1
#define SDL_MIX_MAXVOLUME 128

static inline int SDL_Init(Uint32 f) { (void) f; return 0; }
static inline void SDL_QuitSubSystem(Uint32 f) { (void) f; }
static inline void SDL_PauseAudio(int p) { (void) p; }

SDL_mutex *SDL_CreateMutex(void);
void SDL_DestroyMutex(SDL_mutex *m);
static inline int SDL_LockMutex(SDL_mutex *m) { (void) m; return 0; }
static inline int SDL_UnlockMutex(SDL_mutex *m) { (void) m; return 0; }
SDL_cond *SDL_CreateCond(void);
void SDL_DestroyCond(SDL_cond *c);
static inline int SDL_CondSignal(SDL_cond *c) { (void) c; return 0; }
/* No other thread will signal: play one device buffer, as the audio thread
 * would while the caller waits. */
int SDL_CondWait(SDL_cond *c, SDL_mutex *m);

/* SDL_MixAudioFormat for AUDIO_S16SYS at full volume: add with clipping. */
void SDL_MixAudioFormat(Uint8 *dst, const Uint8 *src, Uint16 format, Uint32 len, int volume);

#endif
