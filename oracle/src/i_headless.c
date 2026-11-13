// Headless platform layer for the doom-oracle (NOT part of linuxdoom-1.10).
//
// Replaces i_system.c, i_video.c, i_sound.c, i_net.c and s_sound.c with
// stubs. None of these touch play state:
//  - video: D_Display is never called, so nothing is drawn or blitted.
//  - sound: s_sound.c only ever draws from M_Random (the menu/cosmetic RNG,
//    rndindex), never P_Random (prndindex), and keeps no play state; the
//    S_* entry points here do nothing.
//  - net: single player only, exactly the "no -net" branch of the original
//    I_InitNetwork.
//  - system: I_GetTime is a counter (only used for wipes/timedemo/net
//    timing, none of which the oracle runs); I_Quit exits 0 without saving
//    defaults; I_Error prints to stderr and exits 1.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "doomdef.h"
#include "doomstat.h"
#include "d_net.h"
#include "m_argv.h"
#include "i_system.h"
#include "i_video.h"
#include "i_sound.h"
#include "i_net.h"
#include "s_sound.h"
#include "sounds.h"

extern void Oracle_Flush (void);

// ---------------------------------------------------------------- system

// 64 MB zone instead of the original 6 MB: fewer PU_CACHE purges, which
// never affect play state (purged lumps are just reloaded).
#define ORACLE_HEAPSIZE (64*1024*1024)

void I_Init (void) {}

byte* I_ZoneBase (int* size)
{
    *size = ORACLE_HEAPSIZE;
    return (byte *) malloc (*size);
}

int I_GetTime (void)
{
    static int t = 0;
    return t++;
}

void I_StartFrame (void) {}
void I_StartTic (void) {}

static ticcmd_t emptycmd;
ticcmd_t* I_BaseTiccmd (void) { return &emptycmd; }

void I_Quit (void)
{
    Oracle_Flush ();
    exit (0);
}

byte* I_AllocLow (int length)
{
    return (byte *) calloc (1, length);
}

void I_Tactile (int on, int off, int total) {}

void I_Error (char *error, ...)
{
    va_list argptr;
    Oracle_Flush ();
    va_start (argptr, error);
    fprintf (stderr, "Error: ");
    vfprintf (stderr, error, argptr);
    fprintf (stderr, "\n");
    va_end (argptr);
    fflush (stderr);
    exit (1);
}

// ---------------------------------------------------------------- video

void I_InitGraphics (void) {}
void I_ShutdownGraphics (void) {}
void I_SetPalette (byte* palette) {}
void I_UpdateNoBlit (void) {}
void I_FinishUpdate (void) {}
void I_WaitVBL (int count) {}
void I_ReadScreen (byte* scr) {}
void I_BeginRead (void) {}
void I_EndRead (void) {}

// ---------------------------------------------------------------- sound (i_sound.c)

FILE*	sndserver = 0;
char*	sndserver_filename = "sndserver";
int	mb_used = 2;

void I_InitSound () {}
void I_UpdateSound (void) {}
void I_SubmitSound (void) {}
void I_ShutdownSound (void) {}
void I_SetChannels () {}
int  I_GetSfxLumpNum (sfxinfo_t* sfxinfo) { return 0; }
int  I_StartSound (int id, int vol, int sep, int pitch, int priority) { return id; }
void I_StopSound (int handle) {}
int  I_SoundIsPlaying (int handle) { return 0; }
void I_UpdateSoundParams (int handle, int vol, int sep, int pitch) {}
void I_InitMusic (void) {}
void I_ShutdownMusic (void) {}
void I_SetMusicVolume (int volume) {}
void I_PauseSong (int handle) {}
void I_ResumeSong (int handle) {}
int  I_RegisterSong (void* data) { return 1; }
void I_PlaySong (int handle, int looping) {}
void I_StopSong (int handle) {}
void I_UnRegisterSong (int handle) {}

// ---------------------------------------------------------------- sound (s_sound.c)

int snd_SfxVolume = 15;
int snd_MusicVolume = 15;
int numChannels;

void S_Init (int sfxVolume, int musicVolume) {}
void S_Start (void) {}
void S_StartSound (void* origin, int sound_id) {}
void S_StartSoundAtVolume (void* origin, int sound_id, int volume) {}
void S_StopSound (void* origin) {}
void S_StartMusic (int music_id) {}
void S_ChangeMusic (int music_id, int looping) {}
void S_StopMusic (void) {}
void S_PauseSound (void) {}
void S_ResumeSound (void) {}
void S_UpdateSounds (void* listener) {}
void S_SetMusicVolume (int volume) {}
void S_SetSfxVolume (int volume) {}

// ---------------------------------------------------------------- net

void I_InitNetwork (void)
{
    doomcom = malloc (sizeof (*doomcom));
    memset (doomcom, 0, sizeof (*doomcom));
    doomcom->ticdup = 1;
    doomcom->extratics = 0;
    netgame = false;
    doomcom->id = DOOMCOM_ID;
    doomcom->numplayers = doomcom->numnodes = 1;
    doomcom->deathmatch = false;
    doomcom->consoleplayer = 0;
}

void I_NetCmd (void) {}
