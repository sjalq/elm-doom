// doom-oracle driver (NOT part of linuxdoom-1.10; replaces i_main.c).
//
//   doom-oracle -iwad <wad> -playdemo <DEMOn lump | demo file> [-maxtics N]
//   doom-oracle -iwad <wad> -ticcmds <file> [-maxtics N]
//
// Prints one state line (format: doom/src/Doom/StateHash.elm) to stdout
// after every P_Ticker call that ran a level tic, and nothing else to
// stdout: everything the game itself prints goes to stderr (stdout's file
// descriptor is pointed at stderr, the state lines go to a private dup of
// the original stdout).
//
// -maxtics N stops after N state lines.
//
// -ticcmds file format: a first text line "skill episode map" (skill 0..4,
// skill_t as in a demo header: sk_baby = 0, sk_medium = 2), then 4 bytes
// per tic: forwardmove (signed), sidemove (signed), angleturn high byte,
// buttons. It is turned into an in-memory v1.9 demo (single player, no
// flags) and played through the normal demo path, so both modes share one
// code path; gen_ticcmds.py --lmp writes the same demo as a file.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// not <unistd.h>: its close() clashes with p_spec.h's "close" enumerator
extern int dup (int);
extern int dup2 (int, int);
#include <sys/stat.h>

#include "doomdef.h"
#include "doomstat.h"
#include "m_argv.h"
#include "d_main.h"
#include "p_local.h"
#include "r_state.h"
#include "info.h"
#include "m_random.h"
#include "i_system.h"

extern int	prndindex;	// m_random.c

// read by G_DoPlayDemo (g_game.c) instead of the lump when set
byte*	oracle_demodata = NULL;

static FILE*	oracle_out = NULL;
static int	oracle_maxtics = -1;
static int	oracle_tics = 0;

void Oracle_Flush (void)
{
    if (oracle_out)
	fflush (oracle_out);
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
    *d++ = VERSION;
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

int main (int argc, char** argv)
{
    int		i, j;
    char**	args;
    char*	playdemo = NULL;
    char*	ticcmds = NULL;
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
	if (!strcmp (argv[i], "-playdemo") && i + 1 < argc)
	    playdemo = argv[i + 1];
	args[j++] = argv[i];
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
    else if (!playdemo)
    {
	fprintf (stderr,
		 "usage: doom-oracle -iwad <wad> (-playdemo <lump|file> | "
		 "-ticcmds <file>) [-maxtics N]\n");
	return 2;
    }
    args[j] = NULL;

    myargc = j;
    myargv = args;

    if (oracle_maxtics == 0)
	return 0;

    D_DoomMain ();
    return 0;
}
