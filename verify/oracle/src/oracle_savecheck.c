// doom-oracle saved-game check (NOT part of linuxdoom-1.10).
//
//   doom-oracle -iwad <wad> -playdemo <demo> -savecheck <leveltime> <out>
//
// At the start of the first G_Ticker in a level whose leveltime is the one
// given (with no game action pending), this runs the original G_DoSaveGame
// (to doomsav0.dsg in the current directory), copies that file to <out>,
// then runs G_LoadGame + G_DoLoadGame on it and turns demo playback back on
// (G_InitNew turns it off), so the rest of the demo's ticcmds are played
// into the loaded game. The state lines go on as usual, so they show how the
// game continues after a load.
//
// The save is already in the 32-bit x86 layout (p_saveg.c), so <out> can be
// compared byte for byte with what src/Doom/Save.elm writes
// (tools/savecheck does the Elm side). A save bigger than SAVEGAMESIZE
// stops the oracle with "Savegame buffer overrun", as the original does.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "doomdef.h"
#include "doomstat.h"
#include "m_argv.h"
#include "g_game.h"
#include "i_system.h"

void	G_DoSaveGame (void);		// g_game.c (not in g_game.h)

extern int	savegameslot;		// g_game.c
extern char	savedescription[32];	// g_game.c

static int	checked = 0;

static void Copy (const char* from, const char* to)
{
    FILE*	in = fopen (from, "rb");
    FILE*	out = fopen (to, "wb");
    char	buf[4096];
    size_t	n;

    if (!in || !out)
	I_Error ("savecheck: can't copy %s to %s", from, to);
    while ((n = fread (buf, 1, sizeof buf, in)) > 0)
	fwrite (buf, 1, n, out);
    fclose (in);
    fclose (out);
}

// Called at the very start of G_Ticker.
void Oracle_SaveCheck (void)
{
    int		p;

    if (checked)
	return;
    p = M_CheckParm ("-savecheck");
    if (!p || p + 2 >= myargc)
	return;
    if (gamestate != GS_LEVEL || gameaction != ga_nothing
	|| leveltime != atoi (myargv[p + 1]))
	return;
    checked = 1;

    savegameslot = 0;
    strcpy (savedescription, "SAVECHECK");
    G_DoSaveGame ();
    Copy ("doomsav0.dsg", myargv[p + 2]);

    G_LoadGame ("doomsav0.dsg");
    G_DoLoadGame ();
    demoplayback = true;
}
