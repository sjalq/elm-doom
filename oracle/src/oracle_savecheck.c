// doom-oracle saved-game check (NOT part of linuxdoom-1.10).
//
//   doom-oracle -iwad <wad> -playdemo DEMOn -savecheck <leveltime> <out32>
//
// At the start of the first G_Ticker in a level whose leveltime is the one
// given (with no game action pending), this runs the original G_DoSaveGame
// (to doomsav0.dsg in the current directory), then G_LoadGame +
// G_DoLoadGame on that file, and turns demo playback back on (G_InitNew
// turns it off) so the rest of the demo's ticcmds are played into the
// loaded game. The state lines go on as usual, so they show how the game
// continues after a load.
//
// The oracle is a 64-bit program, so its doomsav0.dsg has 64-bit struct
// layouts (8-byte pointers and the padding they bring). <out32> is the same
// save rewritten field by field in the 32-bit x86 layout linuxdoom-1.10 was
// written for, with the conventions doom/src/Doom/Save.elm uses for the
// pointers the loader never reads: every pointer is 0, except a thinker's
// `function`, which is 1 when it is set and 0 when NULL (a crusher in
// stasis); padding bytes are 0. Indices the original stores in pointer
// fields (states, player numbers, sectors) are kept. So <out32> can be
// compared byte for byte with what Doom.Save.save writes. If <out32> is
// bigger than SAVEGAMESIZE, this stops with "Savegame buffer overrun", as
// the original's G_DoSaveGame does.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "doomdef.h"
#include "doomstat.h"
#include "m_argv.h"
#include "g_game.h"
#include "p_local.h"
#include "r_state.h"
#include "i_system.h"

void	G_DoSaveGame (void);		// g_game.c (not in g_game.h)

extern int	savegameslot;		// g_game.c
extern char	savedescription[32];	// g_game.c

static int	checked = 0;

static FILE*	out;
static long	outlen;

static void W8 (int v)	{ fputc (v & 255, out); outlen++; }
static void W16 (int v)	{ W8 (v); W8 (v >> 8); }
static void W32 (int v)	{ W16 (v); W16 (v >> 16); }
static void WPad (void)	{ while (outlen & 3) W8 (0); }
static void WThinker (thinker_t* t) { W32 (0); W32 (0); W32 (t->function.acv ? 1 : 0); }

#define I(p)	((int) (intptr_t) (p))

static byte*	in;
static byte*	in_p;
#define RPAD()	in_p += (4 - ((intptr_t) (in_p - in) & 3)) & 3
#define READ(x)	(memcpy (&(x), in_p, sizeof (x)), in_p += sizeof (x))

static void Convert (const char* from, const char* to)
{
    FILE*	f = fopen (from, "rb");
    long	len;
    int		i, j, k, ingame[MAXPLAYERS];
    byte	tclass;

    if (!f)
	I_Error ("savecheck: can't read %s", from);
    fseek (f, 0, SEEK_END);
    len = ftell (f);
    fseek (f, 0, SEEK_SET);
    in = in_p = malloc (len);
    if (fread (in, 1, len, f) != (size_t) len)
	I_Error ("savecheck: short read");
    fclose (f);

    out = fopen (to, "wb");
    outlen = 0;
    if (!out)
	I_Error ("savecheck: can't write %s", to);

    // description, version, skill, episode, map, playeringame, leveltime
    for (i = 0 ; i < 24 + 16 + 3 ; i++)
	W8 (*in_p++);
    for (i = 0 ; i < MAXPLAYERS ; i++)
	W8 (ingame[i] = *in_p++);
    for (i = 0 ; i < 3 ; i++)
	W8 (*in_p++);

    for (i = 0 ; i < MAXPLAYERS ; i++)
    {
	player_t p;
	if (!ingame[i])
	    continue;
	RPAD ();
	READ (p);
	WPad ();
	W32 (0);
	W32 (p.playerstate);
	W8 (p.cmd.forwardmove); W8 (p.cmd.sidemove);
	W16 (p.cmd.angleturn); W16 (p.cmd.consistancy);
	W8 (p.cmd.chatchar); W8 (p.cmd.buttons);
	W32 (p.viewz); W32 (p.viewheight); W32 (p.deltaviewheight); W32 (p.bob);
	W32 (p.health); W32 (p.armorpoints); W32 (p.armortype);
	for (j = 0 ; j < NUMPOWERS ; j++) W32 (p.powers[j]);
	for (j = 0 ; j < NUMCARDS ; j++) W32 (p.cards[j]);
	W32 (p.backpack);
	for (j = 0 ; j < MAXPLAYERS ; j++) W32 (p.frags[j]);
	W32 (p.readyweapon); W32 (p.pendingweapon);
	for (j = 0 ; j < NUMWEAPONS ; j++) W32 (p.weaponowned[j]);
	for (j = 0 ; j < NUMAMMO ; j++) W32 (p.ammo[j]);
	for (j = 0 ; j < NUMAMMO ; j++) W32 (p.maxammo[j]);
	W32 (p.attackdown); W32 (p.usedown); W32 (p.cheats); W32 (p.refire);
	W32 (p.killcount); W32 (p.itemcount); W32 (p.secretcount);
	W32 (0);	// message
	W32 (p.damagecount); W32 (p.bonuscount);
	W32 (0);	// attacker
	W32 (p.extralight); W32 (p.fixedcolormap); W32 (p.colormap);
	for (j = 0 ; j < NUMPSPRITES ; j++)
	{
	    W32 (I (p.psprites[j].state));
	    W32 (p.psprites[j].tics);
	    W32 (p.psprites[j].sx);
	    W32 (p.psprites[j].sy);
	}
	W32 (p.didsecret);
    }

    // P_ArchiveWorld: shorts, no padding
    k = 7 * numsectors;
    for (i = 0 ; i < numlines ; i++)
    {
	k += 3;
	for (j = 0 ; j < 2 ; j++)
	    if (lines[i].sidenum[j] != -1)
		k += 5;
    }
    for (i = 0 ; i < k * 2 ; i++)
	W8 (*in_p++);

    // P_ArchiveThinkers
    while ((tclass = *in_p++) != 0)
    {
	mobj_t m;
	if (tclass != 1)
	    I_Error ("savecheck: thinker class %d", tclass);
	W8 (tclass);
	RPAD ();
	READ (m);
	WPad ();
	WThinker (&m.thinker);
	W32 (m.x); W32 (m.y); W32 (m.z);
	W32 (0); W32 (0);	// snext, sprev
	W32 (m.angle); W32 (m.sprite); W32 (m.frame);
	W32 (0); W32 (0); W32 (0);	// bnext, bprev, subsector
	W32 (m.floorz); W32 (m.ceilingz); W32 (m.radius); W32 (m.height);
	W32 (m.momx); W32 (m.momy); W32 (m.momz);
	W32 (m.validcount); W32 (m.type);
	W32 (0);	// info
	W32 (m.tics); W32 (I (m.state)); W32 (m.flags); W32 (m.health);
	W32 (m.movedir); W32 (m.movecount);
	W32 (0);	// target
	W32 (m.reactiontime); W32 (m.threshold);
	W32 (I (m.player)); W32 (m.lastlook);
	W16 (m.spawnpoint.x); W16 (m.spawnpoint.y); W16 (m.spawnpoint.angle);
	W16 (m.spawnpoint.type); W16 (m.spawnpoint.options);
	W16 (0);
	W32 (0);	// tracer
    }
    W8 (0);

    // P_ArchiveSpecials
    while ((tclass = *in_p++) != 7)
    {
	W8 (tclass);
	RPAD ();
	WPad ();
	switch (tclass)
	{
	  case 0: {
	    ceiling_t c; READ (c); WThinker (&c.thinker);
	    W32 (c.type); W32 (I (c.sector)); W32 (c.bottomheight); W32 (c.topheight);
	    W32 (c.speed); W32 (c.crush); W32 (c.direction); W32 (c.tag); W32 (c.olddirection);
	    break; }
	  case 1: {
	    vldoor_t d; READ (d); WThinker (&d.thinker);
	    W32 (d.type); W32 (I (d.sector)); W32 (d.topheight); W32 (d.speed);
	    W32 (d.direction); W32 (d.topwait); W32 (d.topcountdown);
	    break; }
	  case 2: {
	    floormove_t fl; READ (fl); WThinker (&fl.thinker);
	    W32 (fl.type); W32 (fl.crush); W32 (I (fl.sector)); W32 (fl.direction);
	    W32 (fl.newspecial); W16 (fl.texture); W16 (0);
	    W32 (fl.floordestheight); W32 (fl.speed);
	    break; }
	  case 3: {
	    plat_t p; READ (p); WThinker (&p.thinker);
	    W32 (I (p.sector)); W32 (p.speed); W32 (p.low); W32 (p.high);
	    W32 (p.wait); W32 (p.count); W32 (p.status); W32 (p.oldstatus);
	    W32 (p.crush); W32 (p.tag); W32 (p.type);
	    break; }
	  case 4: {
	    lightflash_t l; READ (l); WThinker (&l.thinker);
	    W32 (I (l.sector)); W32 (l.count); W32 (l.maxlight); W32 (l.minlight);
	    W32 (l.maxtime); W32 (l.mintime);
	    break; }
	  case 5: {
	    strobe_t s; READ (s); WThinker (&s.thinker);
	    W32 (I (s.sector)); W32 (s.count); W32 (s.minlight); W32 (s.maxlight);
	    W32 (s.darktime); W32 (s.brighttime);
	    break; }
	  case 6: {
	    glow_t g; READ (g); WThinker (&g.thinker);
	    W32 (I (g.sector)); W32 (g.minlight); W32 (g.maxlight); W32 (g.direction);
	    break; }
	  default:
	    I_Error ("savecheck: special class %d", tclass);
	}
    }
    W8 (7);
    W8 (*in_p++);	// 0x1d

    fclose (out);
    free (in);

    // G_DoSaveGame's check, on the size the 32-bit original would have had
    if (outlen > 0x2c000)
	I_Error ("Savegame buffer overrun");
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
    Convert ("doomsav0.dsg", myargv[p + 2]);

    G_LoadGame ("doomsav0.dsg");
    G_DoLoadGame ();
    demoplayback = true;
}
