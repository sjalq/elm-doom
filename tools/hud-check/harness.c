// Status bar / HU harness: after each tic, ST_Drawer onto a persistent
// screens[0], HU_Drawer onto a cleared screen, print hashes per frame.
// Keys are fed to ST_Responder / HU_Responder by a script.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doomdef.h"
#include "doomstat.h"
#include "d_event.h"
#include "m_argv.h"
#include "v_video.h"
#include "st_stuff.h"
#include "hu_stuff.h"
#include "g_game.h"
#include "am_map.h"

extern boolean automapactive;
extern int st_palette_harness (void);
static FILE* out;
static int wipegs = GS_DEMOSCREEN;
static byte hu[SCREENWIDTH * SCREENHEIGHT];

typedef struct { int tic; const char* keys; } keyscript_t;

// must match HudCheck.elm
static keyscript_t script[] = {
    { 60, "iddqd" }, { 200, "iddqd" }, { 300, "idbeholds" }, { 700, "idkfa" },
    { 900, "idbeholdr" }, { 1300, "idbeholdv" }, { 1400, "idchoppers" },
    { 1500, "idmypos" }, { 1600, "idmus12" }, { 1700, "idbehold" },
    { 1800, "iiddqd" }, { 1900, "idspispopd" }, { 2000, "idclip" },
    { 2100, "idbeholdl" }, { 2200, "idbeholda" }, { 2300, "idbeholdi" },
    { 2400, "idmus00" }, { 2500, "idfa" }, { 0, NULL }
};

// fullscreen and automap windows, by gametic: must match HudCheck.elm
static int Fullscreen (int t) { return t >= 1000 && t < 1100; }
static int Automap (int t) { return (t >= 1050 && t < 1150) || (t >= 2600 && t < 2700); }

static unsigned H (unsigned h, int v) { return h * 31u + (unsigned) v; }

static void Key (int c)
{
    event_t ev;
    ev.type = ev_keydown;
    ev.data1 = c;
    if (!HU_Responder (&ev))
	ST_Responder (&ev);
    ev.type = ev_keyup;
    HU_Responder (&ev);
    ST_Responder (&ev);
}

void Harness_MusicName (const char* n)
{
    if (out)
	fprintf (out, "music %s\n", n);
}

void Harness_BeforeTic (void)
{
    int i;
    const char* s;
    if (!out)
    {
	int p = M_CheckParm ("-hudout");
	out = fopen (p ? myargv[p + 1] : "hud.txt", "w");
    }
    for (i = 0 ; script[i].keys ; i++)
	if (script[i].tic == gametic && gamestate == GS_LEVEL && !netgame)
	    for (s = script[i].keys ; *s ; s++)
		Key (*s);
    if (gametic == 1650 && gamestate == GS_LEVEL && !netgame)
	Key (KEY_ENTER);
    // -clev: an invalid and a valid level warp, then stop at 3400
    if (M_CheckParm ("-clev") && gamestate == GS_LEVEL && !netgame
	&& (gametic == 2850 || gametic == 2900))
	for (s = gametic == 2850 ? "idclev51" : "idclev13" ; *s ; s++)
	    Key (*s);
}

void Harness_AfterTic (void)
{
    static int lastam = 0;
    int wipe, am, fs, x, y;
    unsigned hs = 0, hh = 0;
    char path[64];

    wipe = gamestate != wipegs;
    wipegs = gamestate;
    if (gamestate != GS_LEVEL || !gametic)
	return;
    am = Automap (gametic);
    fs = Fullscreen (gametic);
    if (am && !lastam)
    {
	event_t ev = { ev_keyup, AM_MSGENTERED };
	ST_Responder (&ev);
    }
    lastam = am;
    automapactive = am;
    ST_Drawer (fs, wipe);
    memcpy (hu, screens[0], sizeof hu);
    memset (screens[0], 0, SCREENWIDTH * 168);
    HU_Drawer ();
    automapactive = 0;
    for (y = 168 ; y < 200 ; y++)
	for (x = 0 ; x < 320 ; x++)
	    hs = H (hs, screens[0][y * 320 + x]);
    for (y = 0 ; y < 168 ; y++)
	for (x = 0 ; x < 320 ; x++)
	    hh = H (hh, screens[0][y * 320 + x]);
    fprintf (out, "%d %d %u %u\n", gametic, st_palette_harness (), hs, hh);
    if (M_CheckParm ("-dumpall") || (gametic % 500) == 0)
    {
	FILE* f;
	sprintf (path, "frames/c%05d.raw", gametic);
	f = fopen (path, "wb");
	if (f) { fwrite (screens[0], 1, 64000, f); fclose (f); }
    }
    // keep the status bar rows; the top rows get cleared again next frame
    memcpy (screens[0], hu, SCREENWIDTH * 168);
    fflush (out);
    if (M_CheckParm ("-clev") && gametic >= 3400)
	exit (0);
}
