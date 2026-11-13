/* A stand-in for the parts of SDL_mixer that Chocolate Doom's OPL driver uses
 * (see SDL.h). The mixer is already "open" at the rate the renderer chose. */
#ifndef REF_SDL_MIXER_H
#define REF_SDL_MIXER_H
#include "SDL.h"

#define MIX_CHANNEL_POST (-2)

typedef void (*Mix_EffectFunc_t)(int chan, void *stream, int len, void *udata);
typedef void (*Mix_EffectDone_t)(int chan, void *udata);

int Mix_QuerySpec(int *frequency, Uint16 *format, int *channels);
int Mix_OpenAudioDevice(int frequency, Uint16 format, int channels, int chunksize,
                        const char *device, int allowed_changes);
static inline void Mix_CloseAudio(void) { }
static inline void Mix_HookMusic(void (*f)(void *, Uint8 *, int), void *arg) { (void) f; (void) arg; }
int Mix_RegisterEffect(int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d, void *arg);
static inline const char *Mix_GetError(void) { return "no SDL here"; }

#endif
