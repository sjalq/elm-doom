/* Byte-order helpers (SDL_endian.h) for a little-endian host. */
#ifndef REF_SDL_ENDIAN_H
#define REF_SDL_ENDIAN_H
#include <stdint.h>
#define SDL_SwapLE16(x) ((uint16_t) (x))
#define SDL_SwapLE32(x) ((uint32_t) (x))
#define SDL_SwapBE16(x) ((uint16_t) ((((uint16_t) (x)) >> 8) | (((uint16_t) (x)) << 8)))
#define SDL_SwapBE32(x) ((uint32_t) (__builtin_bswap32((uint32_t) (x))))
#endif
