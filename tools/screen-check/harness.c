// screen-check harness (NOT part of linuxdoom-1.10): runs the original
// intermission (wi_stuff.c) or finale (f_finale.c) on its own and dumps
// screens[0] after every tic, for tools/screen-check to compare with
// the Elm port pixel for pixel. Built from verify/oracle/src (see Makefile).
//
//   harness -iwad WAD [-file PWAD] -out PREFIX -tics N [-press T,T..] [-key T,T..]
//           ( -wi EPSD LAST NEXT DIDSECRET MAXK MAXI MAXS PARTIME PNUM
//             [-net] [-dm] [-plyr I K IT S TIME F0 F1 F2 F3]...
//           | -finale EPISODE MAP )
//
// Writes PREFIX.frames (64000 bytes per tic, after WI_Ticker/WI_Drawer or
// F_Responder/F_Ticker/F_Drawer) and PREFIX.sounds ("tic sfx NAME",
// "tic music NAME LOOPING"). -press: player 0 holds BT_USE on those tics
// (and lets go on the others). -key: a key-down event goes to F_Responder
// before those tics. Stops after N tics, or after the tic the intermission
// calls G_WorldDone.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "doomdef.h"
#include "doomstat.h"
#include "d_event.h"
#include "m_argv.h"
#include "z_zone.h"
#include "w_wad.h"
#include "v_video.h"
#include "r_main.h"
#include "hu_stuff.h"
#include "wi_stuff.h"
#include "f_finale.h"
#include "g_game.h"
#include "sounds.h"
#include "m_random.h"

void R_Init (void);
void P_Init (void);

// symbols the oracle's copy of the game expects from oracle_main.c
byte*	oracle_demodata = NULL;
boolean	oracle_full = false;
void Oracle_Flush (void) {}
void Oracle_LevelTic (void) {}
void Oracle_Overflow (const char* what, int count, int capacity) {}
void Oracle_BeginIteration (void) {}
void Oracle_EndIteration (void) {}
void Oracle_SetPalette (byte* palette) {}
void Oracle_FinishUpdate (void) {}
void Oracle_SoundLog (const char* fmt, ...) {}
void Oracle_SoundLogSfxVolume (int volume) {}

// s_sound.c is left out (it needs a level to listen from); these stubs log what
// the screens ask for, and S_StartSound draws the pitch M_Random as it does
int	snd_SfxVolume = 15;
int	snd_MusicVolume = 15;
int	numChannels = 8;
int	oracle_snd_channel = -1;
void S_Init (int sfxVolume, int musicVolume) {}
void S_Start (void) {}
void S_StartSoundAtVolume (void* origin, int id, int volume) {}
void S_StopSound (void* origin) {}
void S_PauseSound (void) {}
void S_ResumeSound (void) {}
void S_UpdateSounds (void* listener) {}
void S_SetMusicVolume (int volume) {}
void S_SetSfxVolume (int volume) {}
void S_StopMusic (void) {}

static FILE*	soundlog;
static int	tic;

// S_StartSoundAtVolume's pitch variation draw (an ambient sound is always heard)
void S_StartSound (void* origin, int id)
{
    int byte = (id == sfx_itemup || id == sfx_tink) ? 0 : M_Random ();
    fprintf (soundlog, "%d sfx %s %d\n", tic, S_sfx[id].name, byte);
}

void S_ChangeMusic (int id, int looping)
{
    fprintf (soundlog, "%d music %s %d\n", tic, S_music[id].name, looping);
}

void S_StartMusic (int id)
{
    S_ChangeMusic (id, false);
}

static int InList (const char* list, int t)
{
    const char* p = list;
    if (!list)
	return 0;
    while (*p)
    {
	if (atoi (p) == t)
	    return 1;
	while (*p && *p != ',')
	    p++;
	if (*p == ',')
	    p++;
    }
    return 0;
}

int main (int argc, char** argv)
{
    char*		wadfiles[3] = { NULL, NULL, NULL };
    char*		out = "out";
    char*		press = NULL;
    char*		keys = NULL;
    int			tics = 100;
    int			wi = 0, fin = 0;
    static wbstartstruct_t wbs;
    int			i, j;
    char		path[512];
    FILE*		frames;

    memset (&wbs, 0, sizeof (wbs));
    for (i = 1 ; i < argc ; i++)
    {
	if (!strcmp (argv[i], "-iwad")) wadfiles[0] = argv[++i];
	else if (!strcmp (argv[i], "-file")) wadfiles[1] = argv[++i];
	else if (!strcmp (argv[i], "-out")) out = argv[++i];
	else if (!strcmp (argv[i], "-tics")) tics = atoi (argv[++i]);
	else if (!strcmp (argv[i], "-press")) press = argv[++i];
	else if (!strcmp (argv[i], "-key")) keys = argv[++i];
	else if (!strcmp (argv[i], "-net")) netgame = true;
	else if (!strcmp (argv[i], "-dm")) deathmatch = 1;
	else if (!strcmp (argv[i], "-wi"))
	{
	    wi = 1;
	    wbs.epsd = atoi (argv[++i]);
	    wbs.last = atoi (argv[++i]);
	    wbs.next = atoi (argv[++i]);
	    wbs.didsecret = atoi (argv[++i]);
	    wbs.maxkills = atoi (argv[++i]);
	    wbs.maxitems = atoi (argv[++i]);
	    wbs.maxsecret = atoi (argv[++i]);
	    wbs.partime = atoi (argv[++i]);
	    wbs.pnum = atoi (argv[++i]);
	}
	else if (!strcmp (argv[i], "-plyr"))
	{
	    int n = atoi (argv[++i]);
	    playeringame[n] = true;
	    wbs.plyr[n].in = true;
	    wbs.plyr[n].skills = atoi (argv[++i]);
	    wbs.plyr[n].sitems = atoi (argv[++i]);
	    wbs.plyr[n].ssecret = atoi (argv[++i]);
	    wbs.plyr[n].stime = atoi (argv[++i]);
	    for (j = 0 ; j < MAXPLAYERS ; j++)
		wbs.plyr[n].frags[j] = atoi (argv[++i]);
	}
	else if (!strcmp (argv[i], "-finale"))
	{
	    fin = 1;
	    gameepisode = atoi (argv[++i]);
	    gamemap = atoi (argv[++i]);
	}
    }
    if (!wadfiles[0] || wi == fin)
    {
	fprintf (stderr, "usage: see harness.c\n");
	return 2;
    }

    myargc = 1;
    myargv = argv;
    Z_Init ();
    W_InitMultipleFiles (wadfiles);
    if (W_CheckNumForName ("MAP01") >= 0)
	gamemode = commercial;
    else if (W_CheckNumForName ("E4M1") >= 0)
	gamemode = retail;
    else if (W_CheckNumForName ("E3M1") >= 0)
	gamemode = registered;
    else
	gamemode = shareware;
    V_Init ();
    R_Init ();
    P_Init ();	// R_InitSprites, for the cast
    HU_Init ();

    snprintf (path, sizeof path, "%s.frames", out);
    frames = fopen (path, "wb");
    snprintf (path, sizeof path, "%s.sounds", out);
    soundlog = fopen (path, "w");

    if (wi)
    {
	gamestate = GS_INTERMISSION;
	WI_Start (&wbs);
    }
    else
    {
	gamestate = GS_FINALE;
	F_StartFinale ();
    }

    for (tic = 0 ; tic < tics ; tic++)
    {
	for (i = 0 ; i < MAXPLAYERS ; i++)
	    players[i].cmd.buttons = (i == 0 && InList (press, tic)) ? BT_USE : 0;
	if (fin && InList (keys, tic))
	{
	    event_t ev;
	    ev.type = ev_keydown;
	    ev.data1 = ' ';
	    ev.data2 = ev.data3 = 0;
	    F_Responder (&ev);
	}
	gameaction = ga_nothing;
	if (wi)
	{
	    WI_Ticker ();
	    WI_Drawer ();
	}
	else
	{
	    F_Ticker ();
	    F_Drawer ();
	}
	fwrite (screens[0], 1, SCREENWIDTH * SCREENHEIGHT, frames);
	if (wi && gameaction == ga_worlddone)
	{
	    tic++;
	    break;
	}
    }
    fclose (frames);
    fclose (soundlog);
    fprintf (stderr, "harness: %d tics\n", tic);
    return 0;
}
