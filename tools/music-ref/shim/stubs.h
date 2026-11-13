/* The few Chocolate Doom engine functions the music files call (deh_main.h,
 * m_misc.h, w_wad.h, z_zone.h, i_system.h all include this). */
#ifndef REF_STUBS_H
#define REF_STUBS_H
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "doomtype.h"

#define PU_STATIC 1
#define PU_CACHE 101

#define DEH_String(s) (s)

void *W_CacheLumpName(const char *name, int tag);
void W_ReleaseLumpName(const char *name);

void *Z_Malloc(int size, int tag, void *user);
void Z_Free(void *ptr);
void Z_Init(void);

void *I_Realloc(void *ptr, size_t size);
void I_Error(const char *error, ...);

char *M_TempFile(const char *s);
boolean M_WriteFile(const char *name, const void *source, int length);
int M_ReadFile(const char *name, byte **buffer);
int M_remove(const char *path);
FILE *M_fopen(const char *filename, const char *mode);
int M_snprintf(char *buf, size_t buf_len, const char *s, ...);
boolean M_StringConcat(char *dest, const char *src, size_t dest_size);

#endif
