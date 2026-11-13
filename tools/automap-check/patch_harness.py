import sys

root = sys.argv[1]

p = root + '/src/g_game.c'
s = open(p).read()
s = s.replace("\tAM_Ticker (); \n", "\tAM_Ticker (); \n\t{ extern void Oracle_AfterAM (void); Oracle_AfterAM (); }\n", 1)
assert 'Oracle_AfterAM' in s
open(p, 'w').write(s)

p = root + '/src/oracle_main.c'
s = open(p).read()
s = s.replace('''	if (!strcmp (argv[i], "-ticcmds")''', '''	if (!strcmp (argv[i], "-amscript") && i + 1 < argc)
	{
	    AmLoad (argv[++i]);
	    continue;
	}
	if (!strcmp (argv[i], "-amout") && i + 1 < argc)
	{
	    amout = argv[++i];
	    continue;
	}
	if (!strcmp (argv[i], "-ticcmds")''', 1)
s = s.replace('''static unsigned int H (''', '''#include "d_event.h"
#include "am_map.h"
extern boolean automapactive;
typedef struct { int tic; char cmd[16]; int arg; } amcmd_t;
static amcmd_t amcmds[10000];
static int namcmds = 0;
static char* amout = "amout";
static void AmLoad (const char* path)
{
    FILE* f = fopen (path, "r");
    char line[128];
    if (!f) { fprintf (stderr, "no amscript\\n"); exit (2); }
    while (fgets (line, sizeof line, f))
    {
	amcmd_t* c = &amcmds[namcmds];
	c->arg = 0;
	if (sscanf (line, "%d %15s %d", &c->tic, c->cmd, &c->arg) >= 2)
	    namcmds++;
    }
    fclose (f);
}
static void AmEvents (void)
{
    int k, i;
    for (k = 0 ; k < namcmds ; k++)
    {
	amcmd_t* c = &amcmds[k];
	if (c->tic != leveltime) continue;
	if (!strcmp (c->cmd, "down") || !strcmp (c->cmd, "up"))
	{
	    event_t ev;
	    ev.type = c->cmd[0] == 'd' ? ev_keydown : ev_keyup;
	    ev.data1 = c->arg;
	    ev.data2 = ev.data3 = 0;
	    AM_Responder (&ev);
	}
	else if (!strcmp (c->cmd, "map"))
	{
	    for (i = 0 ; i < numlines ; i++)
		if (i % 3 != 2) lines[i].flags |= ML_MAPPED;
	}
	else if (!strcmp (c->cmd, "allmap"))
	    players[consoleplayer].powers[pw_allmap] = c->arg;
    }
}
void Oracle_AfterAM (void)
{
    int k;
    for (k = 0 ; k < namcmds ; k++)
    {
	amcmd_t* c = &amcmds[k];
	if (c->tic == leveltime && !strcmp (c->cmd, "dump") && automapactive)
	{
	    char path[512];
	    FILE* f;
	    extern byte* screens[5];
	    AM_Drawer ();
	    snprintf (path, sizeof path, "%s/am%05d.raw", amout, leveltime);
	    f = fopen (path, "wb");
	    fwrite (screens[0], 1, 320 * 168, f);
	    fclose (f);
	}
    }
}

static unsigned int H (''', 1)
s = s.replace('''    oracle_tics++;''', '''    AmEvents ();
    oracle_tics++;''', 1)
assert 'AmEvents ();' in s and 'AmLoad (argv' in s
open(p, 'w').write(s)
