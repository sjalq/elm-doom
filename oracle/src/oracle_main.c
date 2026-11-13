// doom-oracle driver (NOT part of linuxdoom-1.10; replaces i_main.c).
//
// TIC-HASH MODE (round 1, unchanged)
//
//   doom-oracle -iwad <wad> -playdemo <DEMOn lump | demo file> [-maxtics N]
//   doom-oracle -iwad <wad> -ticcmds <file> [-maxtics N]
//
// Prints one state line (format: doom/src/Doom/StateHash.elm) to stdout
// after every P_Ticker call that ran a level tic, and nothing else to
// stdout: everything the game itself prints goes to stderr (stdout's file
// descriptor is pointed at stderr, the state lines go to a private dup of
// the original stdout). No display, no D_Display, no S_UpdateSounds.
//
// -maxtics N stops after N state lines.
//
// -ticcmds file format: a first text line "skill episode map" (skill 0..4,
// skill_t as in a demo header: sk_baby = 0, sk_medium = 2), then 4 bytes
// per tic: forwardmove (signed), sidemove (signed), angleturn high byte,
// buttons. It is turned into an in-memory v1.9 demo (single player, no
// flags) and played through the normal demo path, so both modes share one
// code path; gen_ticcmds.py --lmp writes the same demo as a file.
//
// FULL-GAME MODE
//
//   doom-oracle -full -iwad <wad> [original flags: -warp e m, -skill n,
//       -playdemo X, -nomonsters, -fast, -respawn, -loadgame n, ...]
//       [-iterations N] [-frames FILE [-framesfrom A] [-framesto B]]
//       [-events FILE] [-soundlog FILE] [-savedir DIR] [-set NAME VALUE]...
//
// D_DoomMain runs as in the original: without -playdemo / -warp / -skill /
// -loadgame it goes to D_StartTitle (title loop), -warp/-skill autostart
// G_InitNew, -playdemo plays one demo and quits at its end (singledemo).
// D_DoomLoop is the original loop with singletics set, so iteration k is:
//
//   post the -events lines of iteration k (D_PostEvent, file order)
//   I_StartFrame; I_StartTic; D_ProcessEvents;
//   G_BuildTiccmd (&netcmds[consoleplayer][maketic%BACKUPTICS]);
//   if (advancedemo) D_DoAdvanceDemo (); M_Ticker (); G_Ticker ();
//   gametic++; maketic++;
//   S_UpdateSounds (players[consoleplayer].mo); D_Display ();
//
// Iterations count from 0. NetUpdate does nothing in singletics (d_net.c,
// DOOM-ORACLE: models a machine where no tic boundary falls inside a
// frame), so its calls in D_Display / R_RenderPlayerView neither build
// ticcmds nor drain events nor read the clock. State lines are still
// printed to stdout.
//
// -iterations N  exit 0 after iteration N-1 (after its D_Display).
//
// DETERMINISTIC TIME. I_GetTime returns 0, 1, 2, ...: exactly one more on
// every call, whoever calls it. In -full mode it is called only by:
//   - G_DoLoadLevel (starttime = I_GetTime ()), once per level load;
//   - M_Responder, for ev_mouse (and ev_joystick) events only:
//     "mousewait < I_GetTime ()" once per mouse event, and
//     "mousewait = I_GetTime () + 5" (or + 15) when a move or button
//     is turned into a menu key;
//   - the wipe loop of D_Display;
//   - (timedemo and G_CheckDemoStatus's timedemo report: not used.)
// The wipe loop is
//     wipestart = I_GetTime () - 1;
//     do {
//         do { nowtime = I_GetTime (); tics = nowtime - wipestart; }
//         while (!tics);
//         wipestart = nowtime;
//         done = wipe_ScreenWipe (wipe_Melt, 0, 0, 320, 200, tics);
//         I_UpdateNoBlit (); M_Drawer (); I_FinishUpdate ();
//     } while (!done);
// If the first call returns t, wipestart = t-1, the inner loop's call
// returns t+1, so tics = 2; afterwards each step's single call returns one
// more than the last, so tics = 1. Nothing inside the loop body calls
// I_GetTime. Hence: the FIRST melt step advances the melt by 2 ticks, EVERY
// LATER step by 1, one frame (I_FinishUpdate, with M_Drawer drawn on top)
// per step, until wipe_ScreenWipe reports done; all of these frames happen
// inside one D_Display, i.e. within one loop iteration (no tics run, no
// events are read, no sounds are updated meanwhile). The absolute value of
// the counter only matters to M_Responder's mousewait.
//
// FRAMES. -frames FILE: at every I_FinishUpdate (a normal frame, and each
// step of the wipe loop) a record is appended:
//     u32 little-endian frame counter (0, 1, 2, ...: the number of
//         I_FinishUpdate calls before this one),
//     u8  palette: the PLAYPAL palette (0..13) of the last I_SetPalette
//         call, = (pointer - start of the cached PLAYPAL lump) / 768;
//         0 before the first call,
//     u8  usegamma as it was at that I_SetPalette call (the gamma the
//         original uploaded with that palette; 0 before the first call),
//     64000 bytes: screens[0], row-major 320x200 palette indices.
// -framesfrom A / -framesto B (inclusive, on the frame counter) limit
// which records are written; the counter still counts every frame.
//
// EVENTS. -events FILE: one event per line, '#' starts a comment:
//     <iteration> down <keycode>
//     <iteration> up <keycode>
//     <iteration> mouse <buttons> <dx> <dy>
// all decimal integers. keycode is event_t.data1 of ev_keydown/ev_keyup
// (doomdef.h: lower-case ASCII for printable keys, KEY_* constants, the
// same as doom/src/Doom/Event.elm); mouse is ev_mouse with data1 buttons
// (bit 0 left, 1 right, 2 middle), data2 dx, data3 dy as D_PostEvent
// receives them (no scaling applied). Events of iteration k are posted, in
// file order, at the top of iteration k.
//
// SOUND LOG. -soundlog FILE: one line per call of the headless sound layer
// by the real s_sound.c, "<iteration> " followed by
//     start <channel> <lumpname> <vol> <sep> <pitch> <priority>
//     update <channel> <vol> <sep> <pitch>
//     stop <channel>
//     music <lumpname> <looping>
//     musicstop | musicpause | musicresume
//     musicvolume <v>        (every I_SetMusicVolume call)
//     sfxvolume <v>          (every S_SetSfxVolume call)
// Calls before the first loop iteration are logged with iteration -1. The
// "is playing" rule is in i_headless.c.
//
// SAVES. -savedir DIR: the process works in DIR (created if missing) once
// every input path has been resolved, so the original doomsav<n>.dsg files
// of G_DoSaveGame / G_DoLoadGame / M_ReadSaveStrings / -loadgame n live
// there. Layout: p_saveg.c.
//
// DEFAULTS: the m_misc.c defaults table, never a config file;
// -set NAME VALUE overrides one (m_misc.c).
//
// MEMORY THE ORIGINAL READS WITHOUT DEFINING IT, made canonical (a port
// must do the same to match pixel for pixel):
//   - bytes past the end of a lump or composite texture read as 0
//     (R_DrawColumn's &127 reaches up to 127 bytes past a short column;
//     w_wad.h ORACLE_LUMPPAD);
//   - finesine[-4096..-1] is finetangent[0..4095] (tables.h);
//   - freshly allocated zone memory is 0 (z_zone.c).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
// not <unistd.h>: its close() clashes with p_spec.h's "close" enumerator
extern int dup (int);
extern int dup2 (int, int);
extern int chdir (const char*);
#include <sys/stat.h>
#include <limits.h>

#include "doomdef.h"
#include "doomstat.h"
#include "m_argv.h"
#include "d_main.h"
#include "d_event.h"
#include "p_local.h"
#include "r_state.h"
#include "info.h"
#include "m_random.h"
#include "i_system.h"
#include "v_video.h"
#include "w_wad.h"

extern int	prndindex;	// m_random.c
extern boolean	singletics;	// d_main.c

// read by G_DoPlayDemo (g_game.c) instead of the lump when set
byte*	oracle_demodata = NULL;

boolean	oracle_full = false;	// -full, read by D_DoomLoop

static FILE*	oracle_out = NULL;
static int	oracle_maxtics = -1;
static int	oracle_tics = 0;

static int	oracle_iteration = -1;	// -1 until the loop starts
static int	oracle_maxiterations = -1;

static FILE*	frames_out = NULL;
static int	frames_from = 0;
static int	frames_to = INT_MAX;
static int	frame_count = 0;
static int	cur_palette = 0;
static int	cur_gamma = 0;

static FILE*	sound_out = NULL;

typedef struct
{
    int		iteration;
    event_t	ev;
} scripted_t;

static scripted_t*	script = NULL;
static int		numevents = 0;
static int		nextevent = 0;

void Oracle_Flush (void)
{
    if (oracle_out)
	fflush (oracle_out);
    if (frames_out)
	fflush (frames_out);
    if (sound_out)
	fflush (sound_out);
}

// A fixed-size array of the original went past its end (spechit > 8,
// intercepts > 128, linespeciallist > 64). Vanilla then overwrites
// neighbouring memory, which depends on the binary's memory layout and that
// no port reproduces; the oracle's arrays are larger, so the game goes on
// as if the limit did not exist. Reported on stderr so a mismatch there is
// recognisable.
void Oracle_Overflow (const char* what, int count, int capacity)
{
    fprintf (stderr, "doom-oracle: OVERFLOW %s count %d (gametic %d, leveltime %d)\n",
	     what, count, gametic, leveltime);
    if (count > capacity)
	I_Error ("%s overflowed the oracle's array too", what);
}

static unsigned int H (unsigned int h, int v)
{
    return h * 31u + (unsigned int) v;
}

// Called from G_Ticker right after a P_Ticker call that advanced leveltime.
void Oracle_LevelTic (void)
{
    thinker_t*	th;
    mobj_t*	mo;
    player_t*	p = &players[0];
    int		count = 0;
    unsigned int mh = 0, sh = 0;
    int		i;

    for (th = thinkercap.next ; th != &thinkercap ; th = th->next)
    {
	if (th->function.acp1 != (actionf_p1) P_MobjThinker)
	    continue;
	mo = (mobj_t *) th;
	count++;
	mh = H (mh, mo->x);
	mh = H (mh, mo->y);
	mh = H (mh, mo->z);
	mh = H (mh, (int) mo->angle);
	mh = H (mh, mo->momx);
	mh = H (mh, mo->momy);
	mh = H (mh, mo->momz);
	mh = H (mh, (int) mo->type);
	mh = H (mh, (int) (mo->state - states));
	mh = H (mh, mo->tics);
	mh = H (mh, mo->health);
	mh = H (mh, mo->flags);
    }

    for (i = 0 ; i < numsectors ; i++)
    {
	sh = H (sh, sectors[i].floorheight);
	sh = H (sh, sectors[i].ceilingheight);
	sh = H (sh, sectors[i].lightlevel);
	sh = H (sh, sectors[i].special);
    }

    mo = p->mo;
    fprintf (oracle_out,
	     "%d r%d p%d,%d,%d,%u,%d,%d,%d,%d,%d,%d,%d,%d,%d m%d h%u s%u\n",
	     leveltime, prndindex,
	     mo ? mo->x : 0, mo ? mo->y : 0, mo ? mo->z : 0,
	     mo ? (unsigned int) mo->angle : 0u,
	     mo ? mo->momx : 0, mo ? mo->momy : 0,
	     p->health, p->armorpoints, (int) p->readyweapon,
	     p->ammo[0], p->ammo[1], p->ammo[2], p->ammo[3],
	     count, mh, sh);

    oracle_tics++;
    if (oracle_maxtics >= 0 && oracle_tics >= oracle_maxtics)
    {
	Oracle_Flush ();
	exit (0);
    }
}

// ---------------------------------------------------------------- -full hooks

// top of each D_DoomLoop iteration (-full)
void Oracle_BeginIteration (void)
{
    oracle_iteration++;
    while (nextevent < numevents
	   && script[nextevent].iteration <= oracle_iteration)
    {
	if (script[nextevent].iteration == oracle_iteration)
	    D_PostEvent (&script[nextevent].ev);
	nextevent++;
    }
}

// bottom of each D_DoomLoop iteration (-full)
void Oracle_EndIteration (void)
{
    if (oracle_maxiterations >= 0
	&& oracle_iteration + 1 >= oracle_maxiterations)
    {
	Oracle_Flush ();
	exit (0);
    }
}

void Oracle_SetPalette (byte* palette)
{
    int		lump = W_GetNumForName ("PLAYPAL");
    byte*	base = lumpcache[lump];
    long	off = base ? (long) (palette - base) : -1;

    if (!base || off < 0 || off % 768 || off / 768 > 13)
	I_Error ("I_SetPalette: not a PLAYPAL palette");
    cur_palette = (int) (off / 768);
    cur_gamma = usegamma;
}

void Oracle_FinishUpdate (void)
{
    if (frames_out && frame_count >= frames_from && frame_count <= frames_to)
    {
	byte hdr[6];
	hdr[0] = frame_count & 0xff;
	hdr[1] = (frame_count >> 8) & 0xff;
	hdr[2] = (frame_count >> 16) & 0xff;
	hdr[3] = (frame_count >> 24) & 0xff;
	hdr[4] = cur_palette;
	hdr[5] = cur_gamma;
	fwrite (hdr, 1, 6, frames_out);
	fwrite (screens[0], 1, SCREENWIDTH*SCREENHEIGHT, frames_out);
    }
    frame_count++;
}

void Oracle_SoundLog (const char* fmt, ...)
{
    va_list	ap;
    if (!sound_out)
	return;
    fprintf (sound_out, "%d ", oracle_iteration);
    va_start (ap, fmt);
    vfprintf (sound_out, fmt, ap);
    va_end (ap);
    fputc ('\n', sound_out);
}

void Oracle_SoundLogSfxVolume (int volume)
{
    Oracle_SoundLog ("sfxvolume %d", volume);
}

// ---------------------------------------------------------------- input files

static byte* ReadFile (const char* path, long* len)
{
    FILE*	f = fopen (path, "rb");
    byte*	buf;
    if (!f)
	return NULL;
    fseek (f, 0, SEEK_END);
    *len = ftell (f);
    fseek (f, 0, SEEK_SET);
    buf = malloc (*len + 1);
    if (fread (buf, 1, *len, f) != (size_t) *len)
    {
	fprintf (stderr, "doom-oracle: short read on %s\n", path);
	exit (2);
    }
    fclose (f);
    return buf;
}

static void ReadEvents (const char* path)
{
    FILE*	f = fopen (path, "r");
    char	line[256];
    int		lineno = 0;
    int		cap = 0;
    int		last = -1;

    if (!f)
    {
	fprintf (stderr, "doom-oracle: can't read %s\n", path);
	exit (2);
    }
    while (fgets (line, sizeof (line), f))
    {
	char	kind[16];
	int	it, a, b, c, n;
	char*	hash = strchr (line, '#');
	event_t	ev;

	lineno++;
	if (hash)
	    *hash = 0;
	n = sscanf (line, "%d %15s %d %d %d", &it, kind, &a, &b, &c);
	if (n <= 0)
	    continue;	// blank
	memset (&ev, 0, sizeof (ev));
	if (n == 3 && !strcmp (kind, "down"))
	{
	    ev.type = ev_keydown;
	    ev.data1 = a;
	}
	else if (n == 3 && !strcmp (kind, "up"))
	{
	    ev.type = ev_keyup;
	    ev.data1 = a;
	}
	else if (n == 5 && !strcmp (kind, "mouse"))
	{
	    ev.type = ev_mouse;
	    ev.data1 = a;
	    ev.data2 = b;
	    ev.data3 = c;
	}
	else
	{
	    fprintf (stderr, "doom-oracle: %s:%d: bad event line\n", path, lineno);
	    exit (2);
	}
	if (it < 0 || it < last)
	{
	    fprintf (stderr, "doom-oracle: %s:%d: iterations must be >= 0 "
		     "and in non-decreasing order\n", path, lineno);
	    exit (2);
	}
	last = it;
	if (numevents == cap)
	{
	    cap = cap ? cap * 2 : 64;
	    script = realloc (script, cap * sizeof (*script));
	}
	script[numevents].iteration = it;
	script[numevents].ev = ev;
	numevents++;
    }
    fclose (f);
}

// ticcmd stream -> in-memory v1.9 demo
static byte* TiccmdsToDemo (const char* path)
{
    long	len, i, n;
    byte*	raw = ReadFile (path, &len);
    byte*	demo;
    byte*	d;
    int		skill, episode, map;
    char*	nl;

    if (!raw)
    {
	fprintf (stderr, "doom-oracle: can't read %s\n", path);
	exit (2);
    }
    raw[len] = 0;
    nl = memchr (raw, '\n', len);
    if (!nl || sscanf ((char *) raw, "%d %d %d", &skill, &episode, &map) != 3)
    {
	fprintf (stderr, "doom-oracle: %s: bad header line\n", path);
	exit (2);
    }
    i = (nl - (char *) raw) + 1;
    n = (len - i) / 4;
    if ((len - i) % 4)
	fprintf (stderr, "doom-oracle: %s: trailing %ld bytes ignored\n",
		 path, (len - i) % 4);

    demo = d = malloc (13 + n * 4 + 1);
    *d++ = 109;	// v1.9, the version G_DoPlayDemo plays (DOS behaviour)
    *d++ = skill;
    *d++ = episode;
    *d++ = map;
    *d++ = 0;	// deathmatch
    *d++ = 0;	// respawn
    *d++ = 0;	// fast
    *d++ = 0;	// nomonsters
    *d++ = 0;	// consoleplayer
    *d++ = 1; *d++ = 0; *d++ = 0; *d++ = 0;	// playeringame
    for (; n > 0 ; n--, i += 4)
    {
	if (raw[i] == 0x80)
	{
	    fprintf (stderr, "doom-oracle: forwardmove -128 is the demo end "
		     "marker; stream truncated there\n");
	    break;
	}
	memcpy (d, raw + i, 4);
	d += 4;
    }
    *d++ = 0x80;	// DEMOMARKER
    free (raw);
    return demo;
}

static int FileExists (const char* path)
{
    struct stat st;
    return stat (path, &st) == 0 && S_ISREG (st.st_mode);
}

static FILE* OpenOut (const char* path, const char* mode)
{
    FILE* f = fopen (path, mode);
    if (!f)
    {
	fprintf (stderr, "doom-oracle: can't write %s\n", path);
	exit (2);
    }
    return f;
}

// absolute path, so that -savedir's chdir does not break it
static char* Absolute (const char* path)
{
    char* r = realpath (path, NULL);
    return r ? r : (char *) path;
}

int main (int argc, char** argv)
{
    int		i, j;
    char**	args;
    char*	playdemo = NULL;
    char*	ticcmds = NULL;
    char*	savedir = NULL;
    long	len;

    // state lines -> real stdout; all other game output -> stderr
    oracle_out = fdopen (dup (1), "w");
    dup2 (2, 1);

    args = malloc (sizeof (char *) * (argc + 4));
    for (i = j = 0 ; i < argc ; i++)
    {
	if (!strcmp (argv[i], "-maxtics") && i + 1 < argc)
	{
	    oracle_maxtics = atoi (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-ticcmds") && i + 1 < argc)
	{
	    ticcmds = argv[++i];
	    continue;
	}
	if (!strcmp (argv[i], "-full"))
	{
	    oracle_full = true;
	    continue;
	}
	if (!strcmp (argv[i], "-iterations") && i + 1 < argc)
	{
	    oracle_maxiterations = atoi (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-frames") && i + 1 < argc)
	{
	    frames_out = OpenOut (argv[++i], "wb");
	    continue;
	}
	if (!strcmp (argv[i], "-framesfrom") && i + 1 < argc)
	{
	    frames_from = atoi (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-framesto") && i + 1 < argc)
	{
	    frames_to = atoi (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-events") && i + 1 < argc)
	{
	    ReadEvents (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-soundlog") && i + 1 < argc)
	{
	    sound_out = OpenOut (argv[++i], "w");
	    continue;
	}
	if (!strcmp (argv[i], "-savedir") && i + 1 < argc)
	{
	    savedir = argv[++i];
	    continue;
	}
	if (!strcmp (argv[i], "-playdemo") && i + 1 < argc)
	    playdemo = argv[i + 1];
	args[j++] = argv[i];
	// input files given to the original's own flags
	if ((!strcmp (argv[i], "-iwad") || !strcmp (argv[i], "-file"))
	    && i + 1 < argc)
	{
	    while (i + 1 < argc && argv[i + 1][0] != '-')
		args[j++] = Absolute (argv[++i]);
	}
    }

    if (!oracle_full && (frames_out || sound_out || numevents || savedir
			 || oracle_maxiterations >= 0))
    {
	fprintf (stderr, "doom-oracle: -frames, -soundlog, -events, -savedir "
		 "and -iterations need -full\n");
	return 2;
    }

    if (ticcmds)
    {
	if (playdemo)
	{
	    fprintf (stderr, "doom-oracle: -ticcmds and -playdemo are exclusive\n");
	    return 2;
	}
	oracle_demodata = TiccmdsToDemo (ticcmds);
	args[j++] = "-playdemo";
	args[j++] = "ticcmds";
    }
    else if (playdemo && FileExists (playdemo))
    {
	oracle_demodata = ReadFile (playdemo, &len);
    }
    else if (!playdemo && !oracle_full)
    {
	fprintf (stderr,
		 "usage: doom-oracle -iwad <wad> (-playdemo <lump|file> | "
		 "-ticcmds <file>) [-maxtics N]\n"
		 "       doom-oracle -full -iwad <wad> [...]  (see oracle_main.c)\n");
	return 2;
    }
    args[j] = NULL;

    myargc = j;
    myargv = args;

    if (oracle_maxtics == 0 || oracle_maxiterations == 0)
	return 0;

    if (savedir)
    {
	mkdir (savedir, 0777);
	if (chdir (savedir))
	{
	    fprintf (stderr, "doom-oracle: can't enter %s\n", savedir);
	    return 2;
	}
    }

    // the original's -singletics: one tic per loop iteration
    if (oracle_full)
	singletics = true;

    D_DoomMain ();
    return 0;
}
