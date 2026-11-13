// Headless platform layer for the doom-oracle (NOT part of linuxdoom-1.10).
//
// Replaces i_system.c, i_video.c, i_sound.c and i_net.c. s_sound.c is the
// real one (with logging hooks). None of this touches play state:
//  - video: nothing is shown. I_FinishUpdate hands screens[0] to the -frames
//    dump and I_SetPalette records which PLAYPAL palette was set
//    (oracle_main.c). I_ReadScreen copies screens[0] as i_video.c does (the
//    wipe reads back the frame just drawn).
//  - sound: nothing is mixed. Every I_* call s_sound.c makes is logged
//    (-soundlog) and I_SoundIsPlaying follows a fixed rule (below).
//    s_sound.c only ever draws from M_Random (rndindex, menu/cosmetic),
//    never P_Random (prndindex).
//  - net: single player only, exactly the "no -net" branch of the original
//    I_InitNetwork.
//  - system: I_GetTime is a counter that goes up by exactly 1 per call
//    (see oracle_main.c for what that means for the wipe); I_Quit exits 0
//    without saving defaults; I_Error prints to stderr and exits 1.

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
#include "w_wad.h"
#include "z_zone.h"
#include "v_video.h"

extern void Oracle_Flush (void);

// ---------------------------------------------------------------- system

// 64 MB zone instead of the original 6 MB: fewer PU_CACHE purges, which
// never affect play state (purged lumps are just reloaded).
#define ORACLE_HEAPSIZE (64*1024*1024)

// as i_system.c
void I_Init (void)
{
    I_InitSound ();
}

byte* I_ZoneBase (int* size)
{
    *size = ORACLE_HEAPSIZE;
    return (byte *) malloc (*size);
}

// Deterministic time: 0, 1, 2, ... one more on every call.
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

extern void Oracle_FinishUpdate (void);		// oracle_main.c
extern void Oracle_SetPalette (byte* palette);	// oracle_main.c

void I_InitGraphics (void) {}
void I_ShutdownGraphics (void) {}
void I_SetPalette (byte* palette) { Oracle_SetPalette (palette); }
void I_UpdateNoBlit (void) {}
void I_FinishUpdate (void) { Oracle_FinishUpdate (); }
void I_WaitVBL (int count) {}
void I_ReadScreen (byte* scr)
{
    memcpy (scr, screens[0], SCREENWIDTH*SCREENHEIGHT);
}
void I_BeginRead (void) {}
void I_EndRead (void) {}

// ---------------------------------------------------------------- sound (i_sound.c)
//
// "Is playing" rule. I_StartSound at gametic T (the global gametic at the
// moment of the call) of sfx S, whose sound lump L has sample rate R (u16
// at lump offset 2) and sample count N (u32 at offset 4):
//
//     I_SoundIsPlaying (handle)  <=>  gametic < T + (N*35 + R-1) / R
//
// i.e. the sound lasts its sample length at 35 tics per second, rounded up
// (R 0 counts as 1). I_StopSound ends it at once. L is the lump the
// original mixer plays (i_sound.c getsfx): the link's lump for an alias
// (chgun -> DSPISTOL), else "ds<name>", or DSPISTOL when that is missing.
//
// (The original linuxdoom I_SoundIsPlaying is "gametic < handle", handles
// counting up from 100: a sound "stops" after a time that grows with the
// number of sounds started so far. That is a placeholder, not kept.)

extern int	oracle_snd_channel;			// s_sound.c
extern void	Oracle_SoundLog (const char* fmt, ...);	// oracle_main.c

FILE*	sndserver = 0;
char*	sndserver_filename = "sndserver";
int	mb_used = 2;

static int	sfxlump[NUMSFX];	// lump the mixer would play
static int	sfxtics[NUMSFX];	// its length in tics, rounded up

#define MAXHANDLES	4096
static int	handle_end[MAXHANDLES];	// gametic at which it stops
static int	nexthandle = 0;

void I_InitSound (void)
{
    int		i;
    char	name[20];

    for (i=1 ; i<NUMSFX ; i++)
    {
	sfxinfo_t*	sfx = S_sfx[i].link ? S_sfx[i].link : &S_sfx[i];
	byte*		d;
	int		rate, samples;

	sprintf (name, "ds%s", sfx->name);
	if (W_CheckNumForName (name) == -1)
	    sfxlump[i] = W_GetNumForName ("dspistol");
	else
	    sfxlump[i] = W_GetNumForName (name);
	d = W_CacheLumpNum (sfxlump[i], PU_STATIC);
	rate = d[2] | (d[3] << 8);
	samples = d[4] | (d[5] << 8) | (d[6] << 16) | (d[7] << 24);
	if (rate <= 0)
	    rate = 1;
	sfxtics[i] = (int) (((long long) samples * TICRATE + rate - 1) / rate);
	// like the original precache: s_sound.c only checks that it is set
	S_sfx[i].data = d;
    }
}

void I_UpdateSound (void) {}
void I_SubmitSound (void) {}
void I_ShutdownSound (void) {}
void I_SetChannels () {}

// as i_sound.c
int I_GetSfxLumpNum (sfxinfo_t* sfx)
{
    char namebuf[9];
    sprintf (namebuf, "ds%s", sfx->name);
    return W_GetNumForName (namebuf);
}

int I_StartSound (int id, int vol, int sep, int pitch, int priority)
{
    int	h = nexthandle++ % MAXHANDLES;
    handle_end[h] = gametic + sfxtics[id];
    Oracle_SoundLog ("start %d %.8s %d %d %d %d", oracle_snd_channel,
		     lumpinfo[sfxlump[id]].name, vol, sep, pitch, priority);
    return h;
}

void I_StopSound (int handle)
{
    Oracle_SoundLog ("stop %d", oracle_snd_channel);
    handle_end[handle % MAXHANDLES] = gametic;
}

int I_SoundIsPlaying (int handle)
{
    return gametic < handle_end[handle % MAXHANDLES];
}

void I_UpdateSoundParams (int handle, int vol, int sep, int pitch)
{
    Oracle_SoundLog ("update %d %d %d %d", oracle_snd_channel, vol, sep, pitch);
}

// music: the lump is recognised by the data pointer s_sound.c registers
// (it is W_CacheLumpNum's, so it is in lumpcache)
static int	song_lump = -1;

void I_InitMusic (void) {}
void I_ShutdownMusic (void) {}
void I_SetMusicVolume (int volume) { Oracle_SoundLog ("musicvolume %d", volume); }
void I_PauseSong (int handle) { Oracle_SoundLog ("musicpause"); }
void I_ResumeSong (int handle) { Oracle_SoundLog ("musicresume"); }

int I_RegisterSong (void* data)
{
    int i;
    song_lump = -1;
    for (i=0 ; i<numlumps ; i++)
	if (lumpcache[i] == data)
	{
	    song_lump = i;
	    break;
	}
    return 1;
}

void I_PlaySong (int handle, int looping)
{
    Oracle_SoundLog ("music %.8s %d",
		     song_lump >= 0 ? lumpinfo[song_lump].name : "?", looping);
}

void I_StopSong (int handle) { Oracle_SoundLog ("musicstop"); }
void I_UnRegisterSong (int handle) {}

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
