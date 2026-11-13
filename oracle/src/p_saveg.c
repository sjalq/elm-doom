// DOOM-ORACLE changes to this linuxdoom-1.10 file (doom/oracle; search
// for DOOM-ORACLE below). None of them changes how the game plays:
//  - The original writes players, mobjs and specials by memcpy of the
//    in-memory structs, which on a 64-bit host have 8-byte pointers and
//    different padding. Here every struct is written and read field by
//    field in the layout the 32-bit original had (4-byte pointers and
//    enums, 4-byte alignment; byte offsets are given at each struct), so a
//    savegame has the size and field positions of a 32-bit linuxdoom one
//    and is little-endian as on x86. The values are the original's, with
//    the pointer fields remapped exactly as the original remaps them
//    (state -> index into states[], sector -> index into sectors[],
//    mobj->player -> player number + 1). What cannot be reproduced: a
//    pointer the original wrote as a raw 32-bit address that the loader
//    then ignores or overwrites (thinker prev/next, snext/sprev/bnext/
//    bprev/subsector/info/target/tracer of a mobj, player mo/message/
//    attacker); those are written as 0. A thinker's function pointer is
//    written as 0 when NULL and 1 otherwise (the loader only tests it for
//    NULL: a ceiling or platform in stasis). Struct padding is written as
//    0 (the original wrote whatever the heap held there).
//  - On load, a mobj's tracer is set to NULL: the original kept the raw
//    pointer read from the file (a dangling pointer, undefined behaviour);
//    every other field is set exactly as the original sets it.

// Emacs style mode select   -*- C++ -*-
//-----------------------------------------------------------------------------
//
// $Id:$
//
// Copyright (C) 1993-1996 by id Software, Inc.
//
// This source is available for distribution and/or modification
// only under the terms of the DOOM Source Code License as
// published by id Software. All rights reserved.
//
// The source is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// FITNESS FOR A PARTICULAR PURPOSE. See the DOOM Source Code License
// for more details.
//
// $Log:$
//
// DESCRIPTION:
//	Archiving: SaveGame I/O.
//
//-----------------------------------------------------------------------------

static const char
rcsid[] = "$Id: p_tick.c,v 1.4 1997/02/03 16:47:55 b1 Exp $";

#include "i_system.h"
#include "z_zone.h"
#include "p_local.h"

// State.
#include "doomstat.h"
#include "r_state.h"

byte*		save_p;


// Pads save_p to a 4-byte boundary
//  so that the load/save works on SGI&Gecko.
#define PADSAVEP()	save_p += (4 - ((intptr_t) save_p & 3)) & 3


//
// DOOM-ORACLE: 32-bit little-endian field writers/readers.
//
static void W8 (int v) { *save_p++ = (byte) v; }
static void W16 (int v) { W8 (v); W8 (v >> 8); }
static void W32 (int v) { W16 (v); W16 (v >> 16); }
static int R8 (void) { return *save_p++; }
static int R16 (void) { int v = R8 (); v |= R8 () << 8; return (short) v; }
static int R32 (void)
{
    unsigned int v = (unsigned int) R16 () & 0xffff;
    v |= (unsigned int) R16 () << 16;
    return (int) v;
}

// thinker_t: prev 0, next 4, function 8; 12 bytes
static void WThinker (thinker_t* th)
{
    W32 (0);
    W32 (0);
    W32 (th->function.acv != NULL);
}

// returns whether the function pointer was set
static int RThinker (thinker_t* th)
{
    R32 ();
    R32 ();
    return R32 () != 0;
}



//
// P_ArchivePlayers
//
// player_t, 280 bytes: mo 0, playerstate 4, cmd 8 (forwardmove 8,
// sidemove 9, angleturn 10, consistancy 12, chatchar 14, buttons 15),
// viewz 16, viewheight 20, deltaviewheight 24, bob 28, health 32,
// armorpoints 36, armortype 40, powers[6] 44, cards[6] 68, backpack 92,
// frags[4] 96, readyweapon 112, pendingweapon 116, weaponowned[9] 120,
// ammo[4] 156, maxammo[4] 172, attackdown 188, usedown 192, cheats 196,
// refire 200, killcount 204, itemcount 208, secretcount 212, message 216,
// damagecount 220, bonuscount 224, attacker 228, extralight 232,
// fixedcolormap 236, colormap 240, psprites[2] 244 (state, tics, sx, sy:
// 16 bytes each), didsecret 276.
//
void P_ArchivePlayers (void)
{
    int		i;
    int		j;
    player_t*	p;

    for (i=0 ; i<MAXPLAYERS ; i++)
    {
	if (!playeringame[i])
	    continue;

	PADSAVEP();

	p = &players[i];
	W32 (0);				// mo
	W32 (p->playerstate);
	W8 (p->cmd.forwardmove);
	W8 (p->cmd.sidemove);
	W16 (p->cmd.angleturn);
	W16 (p->cmd.consistancy);
	W8 (p->cmd.chatchar);
	W8 (p->cmd.buttons);
	W32 (p->viewz);
	W32 (p->viewheight);
	W32 (p->deltaviewheight);
	W32 (p->bob);
	W32 (p->health);
	W32 (p->armorpoints);
	W32 (p->armortype);
	for (j=0 ; j<NUMPOWERS ; j++)
	    W32 (p->powers[j]);
	for (j=0 ; j<NUMCARDS ; j++)
	    W32 (p->cards[j]);
	W32 (p->backpack);
	for (j=0 ; j<MAXPLAYERS ; j++)
	    W32 (p->frags[j]);
	W32 (p->readyweapon);
	W32 (p->pendingweapon);
	for (j=0 ; j<NUMWEAPONS ; j++)
	    W32 (p->weaponowned[j]);
	for (j=0 ; j<NUMAMMO ; j++)
	    W32 (p->ammo[j]);
	for (j=0 ; j<NUMAMMO ; j++)
	    W32 (p->maxammo[j]);
	W32 (p->attackdown);
	W32 (p->usedown);
	W32 (p->cheats);
	W32 (p->refire);
	W32 (p->killcount);
	W32 (p->itemcount);
	W32 (p->secretcount);
	W32 (0);				// message
	W32 (p->damagecount);
	W32 (p->bonuscount);
	W32 (0);				// attacker
	W32 (p->extralight);
	W32 (p->fixedcolormap);
	W32 (p->colormap);
	for (j=0 ; j<NUMPSPRITES ; j++)
	{
	    // as the original: an active state becomes its index
	    W32 (p->psprites[j].state ? (int) (p->psprites[j].state - states) : 0);
	    W32 (p->psprites[j].tics);
	    W32 (p->psprites[j].sx);
	    W32 (p->psprites[j].sy);
	}
	W32 (p->didsecret);
    }
}



//
// P_UnArchivePlayers
//
void P_UnArchivePlayers (void)
{
    int		i;
    int		j;
    player_t*	p;

    for (i=0 ; i<MAXPLAYERS ; i++)
    {
	if (!playeringame[i])
	    continue;

	PADSAVEP();

	p = &players[i];
	R32 ();				// mo
	p->playerstate = R32 ();
	p->cmd.forwardmove = (char) R8 ();
	p->cmd.sidemove = (char) R8 ();
	p->cmd.angleturn = R16 ();
	p->cmd.consistancy = R16 ();
	p->cmd.chatchar = R8 ();
	p->cmd.buttons = R8 ();
	p->viewz = R32 ();
	p->viewheight = R32 ();
	p->deltaviewheight = R32 ();
	p->bob = R32 ();
	p->health = R32 ();
	p->armorpoints = R32 ();
	p->armortype = R32 ();
	for (j=0 ; j<NUMPOWERS ; j++)
	    p->powers[j] = R32 ();
	for (j=0 ; j<NUMCARDS ; j++)
	    p->cards[j] = R32 ();
	p->backpack = R32 ();
	for (j=0 ; j<MAXPLAYERS ; j++)
	    p->frags[j] = R32 ();
	p->readyweapon = R32 ();
	p->pendingweapon = R32 ();
	for (j=0 ; j<NUMWEAPONS ; j++)
	    p->weaponowned[j] = R32 ();
	for (j=0 ; j<NUMAMMO ; j++)
	    p->ammo[j] = R32 ();
	for (j=0 ; j<NUMAMMO ; j++)
	    p->maxammo[j] = R32 ();
	p->attackdown = R32 ();
	p->usedown = R32 ();
	p->cheats = R32 ();
	p->refire = R32 ();
	p->killcount = R32 ();
	p->itemcount = R32 ();
	p->secretcount = R32 ();
	R32 ();				// message
	p->damagecount = R32 ();
	p->bonuscount = R32 ();
	R32 ();				// attacker
	p->extralight = R32 ();
	p->fixedcolormap = R32 ();
	p->colormap = R32 ();
	for (j=0 ; j<NUMPSPRITES ; j++)
	{
	    int st = R32 ();
	    // as the original: 0 (NULL) stays NULL, else an index
	    p->psprites[j].state = st ? &states[st] : NULL;
	    p->psprites[j].tics = R32 ();
	    p->psprites[j].sx = R32 ();
	    p->psprites[j].sy = R32 ();
	}
	p->didsecret = R32 ();

	// will be set when unarc thinker
	p->mo = NULL;
	p->message = NULL;
	p->attacker = NULL;
    }
}


//
// P_ArchiveWorld
//
void P_ArchiveWorld (void)
{
    int			i;
    int			j;
    sector_t*		sec;
    line_t*		li;
    side_t*		si;
    short*		put;
	
    put = (short *)save_p;
    
    // do sectors
    for (i=0, sec = sectors ; i<numsectors ; i++,sec++)
    {
	*put++ = sec->floorheight >> FRACBITS;
	*put++ = sec->ceilingheight >> FRACBITS;
	*put++ = sec->floorpic;
	*put++ = sec->ceilingpic;
	*put++ = sec->lightlevel;
	*put++ = sec->special;		// needed?
	*put++ = sec->tag;		// needed?
    }

    
    // do lines
    for (i=0, li = lines ; i<numlines ; i++,li++)
    {
	*put++ = li->flags;
	*put++ = li->special;
	*put++ = li->tag;
	for (j=0 ; j<2 ; j++)
	{
	    if (li->sidenum[j] == -1)
		continue;
	    
	    si = &sides[li->sidenum[j]];

	    *put++ = si->textureoffset >> FRACBITS;
	    *put++ = si->rowoffset >> FRACBITS;
	    *put++ = si->toptexture;
	    *put++ = si->bottomtexture;
	    *put++ = si->midtexture;	
	}
    }
	
    save_p = (byte *)put;
}



//
// P_UnArchiveWorld
//
void P_UnArchiveWorld (void)
{
    int			i;
    int			j;
    sector_t*		sec;
    line_t*		li;
    side_t*		si;
    short*		get;
	
    get = (short *)save_p;
    
    // do sectors
    for (i=0, sec = sectors ; i<numsectors ; i++,sec++)
    {
	sec->floorheight = *get++ << FRACBITS;
	sec->ceilingheight = *get++ << FRACBITS;
	sec->floorpic = *get++;
	sec->ceilingpic = *get++;
	sec->lightlevel = *get++;
	sec->special = *get++;		// needed?
	sec->tag = *get++;		// needed?
	sec->specialdata = 0;
	sec->soundtarget = 0;
    }
    
    // do lines
    for (i=0, li = lines ; i<numlines ; i++,li++)
    {
	li->flags = *get++;
	li->special = *get++;
	li->tag = *get++;
	for (j=0 ; j<2 ; j++)
	{
	    if (li->sidenum[j] == -1)
		continue;
	    si = &sides[li->sidenum[j]];
	    si->textureoffset = *get++ << FRACBITS;
	    si->rowoffset = *get++ << FRACBITS;
	    si->toptexture = *get++;
	    si->bottomtexture = *get++;
	    si->midtexture = *get++;
	}
    }
    save_p = (byte *)get;	
}





//
// Thinkers
//
typedef enum
{
    tc_end,
    tc_mobj

} thinkerclass_t;


//
// DOOM-ORACLE: mobj_t, 156 bytes: thinker 0, x 12, y 16, z 20, snext 24,
// sprev 28, angle 32, sprite 36, frame 40, bnext 44, bprev 48,
// subsector 52, floorz 56, ceilingz 60, radius 64, height 68, momx 72,
// momy 76, momz 80, validcount 84, type 88, info 92, tics 96, state 100,
// flags 104, health 108, movedir 112, movecount 116, target 120,
// reactiontime 124, threshold 128, player 132, lastlook 136,
// spawnpoint 140 (x, y, angle, type, options: 5 shorts), padding 150,
// tracer 152.
//
static void WMobj (mobj_t* mo)
{
    WThinker (&mo->thinker);
    W32 (mo->x);
    W32 (mo->y);
    W32 (mo->z);
    W32 (0);				// snext
    W32 (0);				// sprev
    W32 (mo->angle);
    W32 (mo->sprite);
    W32 (mo->frame);
    W32 (0);				// bnext
    W32 (0);				// bprev
    W32 (0);				// subsector
    W32 (mo->floorz);
    W32 (mo->ceilingz);
    W32 (mo->radius);
    W32 (mo->height);
    W32 (mo->momx);
    W32 (mo->momy);
    W32 (mo->momz);
    W32 (mo->validcount);
    W32 (mo->type);
    W32 (0);				// info
    W32 (mo->tics);
    W32 ((int) (mo->state - states));	// as the original
    W32 (mo->flags);
    W32 (mo->health);
    W32 (mo->movedir);
    W32 (mo->movecount);
    W32 (0);				// target
    W32 (mo->reactiontime);
    W32 (mo->threshold);
    W32 (mo->player ? (int) (mo->player - players) + 1 : 0);	// as the original
    W32 (mo->lastlook);
    W16 (mo->spawnpoint.x);
    W16 (mo->spawnpoint.y);
    W16 (mo->spawnpoint.angle);
    W16 (mo->spawnpoint.type);
    W16 (mo->spawnpoint.options);
    W16 (0);				// padding
    W32 (0);				// tracer
}

static void RMobj (mobj_t* mo)
{
    int pl;

    RThinker (&mo->thinker);
    mo->x = R32 ();
    mo->y = R32 ();
    mo->z = R32 ();
    R32 ();				// snext
    R32 ();				// sprev
    mo->angle = R32 ();
    mo->sprite = R32 ();
    mo->frame = R32 ();
    R32 ();				// bnext
    R32 ();				// bprev
    R32 ();				// subsector
    mo->floorz = R32 ();
    mo->ceilingz = R32 ();
    mo->radius = R32 ();
    mo->height = R32 ();
    mo->momx = R32 ();
    mo->momy = R32 ();
    mo->momz = R32 ();
    mo->validcount = R32 ();
    mo->type = R32 ();
    R32 ();				// info
    mo->tics = R32 ();
    mo->state = &states[R32 ()];
    mo->flags = R32 ();
    mo->health = R32 ();
    mo->movedir = R32 ();
    mo->movecount = R32 ();
    R32 ();				// target
    mo->reactiontime = R32 ();
    mo->threshold = R32 ();
    pl = R32 ();
    mo->player = pl ? &players[pl - 1] : NULL;
    mo->lastlook = R32 ();
    mo->spawnpoint.x = R16 ();
    mo->spawnpoint.y = R16 ();
    mo->spawnpoint.angle = R16 ();
    mo->spawnpoint.type = R16 ();
    mo->spawnpoint.options = R16 ();
    R16 ();				// padding
    R32 ();				// tracer
    mo->target = NULL;
    mo->tracer = NULL;
}


//
// P_ArchiveThinkers
//
void P_ArchiveThinkers (void)
{
    thinker_t*		th;

    // save off the current thinkers
    for (th = thinkercap.next ; th != &thinkercap ; th=th->next)
    {
	if (th->function.acp1 == (actionf_p1)P_MobjThinker)
	{
	    *save_p++ = tc_mobj;
	    PADSAVEP();
	    WMobj ((mobj_t *) th);
	    continue;
	}

	// I_Error ("P_ArchiveThinkers: Unknown thinker function");
    }

    // add a terminating marker
    *save_p++ = tc_end;
}



//
// P_UnArchiveThinkers
//
void P_UnArchiveThinkers (void)
{
    byte		tclass;
    thinker_t*		currentthinker;
    thinker_t*		next;
    mobj_t*		mobj;

    // remove all the current thinkers
    currentthinker = thinkercap.next;
    while (currentthinker != &thinkercap)
    {
	next = currentthinker->next;

	if (currentthinker->function.acp1 == (actionf_p1)P_MobjThinker)
	    P_RemoveMobj ((mobj_t *)currentthinker);
	else
	    Z_Free (currentthinker);

	currentthinker = next;
    }
    P_InitThinkers ();

    // read in saved thinkers
    while (1)
    {
	tclass = *save_p++;
	switch (tclass)
	{
	  case tc_end:
	    return; 	// end of list

	  case tc_mobj:
	    PADSAVEP();
	    mobj = Z_Malloc (sizeof(*mobj), PU_LEVEL, NULL);
	    memset (mobj, 0, sizeof (*mobj));
	    RMobj (mobj);
	    if (mobj->player)
		mobj->player->mo = mobj;
	    P_SetThingPosition (mobj);
	    mobj->info = &mobjinfo[mobj->type];
	    mobj->floorz = mobj->subsector->sector->floorheight;
	    mobj->ceilingz = mobj->subsector->sector->ceilingheight;
	    mobj->thinker.function.acp1 = (actionf_p1)P_MobjThinker;
	    P_AddThinker (&mobj->thinker);
	    break;

	  default:
	    I_Error ("Unknown tclass %i in savegame",tclass);
	}

    }

}


//
// P_ArchiveSpecials
//
enum
{
    tc_ceiling,
    tc_door,
    tc_floor,
    tc_plat,
    tc_flash,
    tc_strobe,
    tc_glow,
    tc_endspecials

} specials_e;


//
// DOOM-ORACLE: 32-bit layouts (thinker_t is 12 bytes at offset 0):
//   ceiling_t 48: type 12, sector 16, bottomheight 20, topheight 24,
//     speed 28, crush 32, direction 36, tag 40, olddirection 44
//   vldoor_t 40: type 12, sector 16, topheight 20, speed 24, direction 28,
//     topwait 32, topcountdown 36
//   floormove_t 44: type 12, crush 16, sector 20, direction 24,
//     newspecial 28, texture 32 (short) + 2 padding, floordestheight 36,
//     speed 40
//   plat_t 56: sector 12, speed 16, low 20, high 24, wait 28, count 32,
//     status 36, oldstatus 40, crush 44, tag 48, type 52
//   lightflash_t 36: sector 12, count 16, maxlight 20, minlight 24,
//     maxtime 28, mintime 32
//   strobe_t 36: sector 12, count 16, minlight 20, maxlight 24,
//     darktime 28, brighttime 32
//   glow_t 28: sector 12, minlight 16, maxlight 20, direction 24
// Sectors are written as their index, as the original does.
//
#define WSEC(s)	W32 ((int) ((s) - sectors))
#define RSEC()	(&sectors[R32 ()])

//
// Things to handle:
//
// T_MoveCeiling, (ceiling_t: sector_t * swizzle), - active list
// T_VerticalDoor, (vldoor_t: sector_t * swizzle),
// T_MoveFloor, (floormove_t: sector_t * swizzle),
// T_LightFlash, (lightflash_t: sector_t * swizzle),
// T_StrobeFlash, (strobe_t: sector_t *),
// T_Glow, (glow_t: sector_t *),
// T_PlatRaise, (plat_t: sector_t *), - active list
//
static void WCeiling (ceiling_t* c)
{
    WThinker (&c->thinker);
    W32 (c->type);
    WSEC (c->sector);
    W32 (c->bottomheight);
    W32 (c->topheight);
    W32 (c->speed);
    W32 (c->crush);
    W32 (c->direction);
    W32 (c->tag);
    W32 (c->olddirection);
}

void P_ArchiveSpecials (void)
{
    thinker_t*		th;
    int			i;

    // save off the current thinkers
    for (th = thinkercap.next ; th != &thinkercap ; th=th->next)
    {
	if (th->function.acv == (actionf_v)NULL)
	{
	    for (i = 0; i < MAXCEILINGS;i++)
		if (activeceilings[i] == (ceiling_t *)th)
		    break;

	    if (i<MAXCEILINGS)
	    {
		*save_p++ = tc_ceiling;
		PADSAVEP();
		WCeiling ((ceiling_t *) th);
	    }
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_MoveCeiling)
	{
	    *save_p++ = tc_ceiling;
	    PADSAVEP();
	    WCeiling ((ceiling_t *) th);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_VerticalDoor)
	{
	    vldoor_t* door = (vldoor_t *) th;
	    *save_p++ = tc_door;
	    PADSAVEP();
	    WThinker (th);
	    W32 (door->type);
	    WSEC (door->sector);
	    W32 (door->topheight);
	    W32 (door->speed);
	    W32 (door->direction);
	    W32 (door->topwait);
	    W32 (door->topcountdown);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_MoveFloor)
	{
	    floormove_t* floor = (floormove_t *) th;
	    *save_p++ = tc_floor;
	    PADSAVEP();
	    WThinker (th);
	    W32 (floor->type);
	    W32 (floor->crush);
	    WSEC (floor->sector);
	    W32 (floor->direction);
	    W32 (floor->newspecial);
	    W16 (floor->texture);
	    W16 (0);			// padding
	    W32 (floor->floordestheight);
	    W32 (floor->speed);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_PlatRaise)
	{
	    plat_t* plat = (plat_t *) th;
	    *save_p++ = tc_plat;
	    PADSAVEP();
	    WThinker (th);
	    WSEC (plat->sector);
	    W32 (plat->speed);
	    W32 (plat->low);
	    W32 (plat->high);
	    W32 (plat->wait);
	    W32 (plat->count);
	    W32 (plat->status);
	    W32 (plat->oldstatus);
	    W32 (plat->crush);
	    W32 (plat->tag);
	    W32 (plat->type);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_LightFlash)
	{
	    lightflash_t* flash = (lightflash_t *) th;
	    *save_p++ = tc_flash;
	    PADSAVEP();
	    WThinker (th);
	    WSEC (flash->sector);
	    W32 (flash->count);
	    W32 (flash->maxlight);
	    W32 (flash->minlight);
	    W32 (flash->maxtime);
	    W32 (flash->mintime);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_StrobeFlash)
	{
	    strobe_t* strobe = (strobe_t *) th;
	    *save_p++ = tc_strobe;
	    PADSAVEP();
	    WThinker (th);
	    WSEC (strobe->sector);
	    W32 (strobe->count);
	    W32 (strobe->minlight);
	    W32 (strobe->maxlight);
	    W32 (strobe->darktime);
	    W32 (strobe->brighttime);
	    continue;
	}

	if (th->function.acp1 == (actionf_p1)T_Glow)
	{
	    glow_t* glow = (glow_t *) th;
	    *save_p++ = tc_glow;
	    PADSAVEP();
	    WThinker (th);
	    WSEC (glow->sector);
	    W32 (glow->minlight);
	    W32 (glow->maxlight);
	    W32 (glow->direction);
	    continue;
	}
    }

    // add a terminating marker
    *save_p++ = tc_endspecials;

}


//
// P_UnArchiveSpecials
//
void P_UnArchiveSpecials (void)
{
    byte		tclass;
    ceiling_t*		ceiling;
    vldoor_t*		door;
    floormove_t*	floor;
    plat_t*		plat;
    lightflash_t*	flash;
    strobe_t*		strobe;
    glow_t*		glow;
    int			active;


    // read in saved thinkers
    while (1)
    {
	tclass = *save_p++;
	switch (tclass)
	{
	  case tc_endspecials:
	    return;	// end of list

	  case tc_ceiling:
	    PADSAVEP();
	    ceiling = Z_Malloc (sizeof(*ceiling), PU_LEVEL, NULL);
	    memset (ceiling, 0, sizeof (*ceiling));
	    active = RThinker (&ceiling->thinker);
	    ceiling->type = R32 ();
	    ceiling->sector = RSEC ();
	    ceiling->bottomheight = R32 ();
	    ceiling->topheight = R32 ();
	    ceiling->speed = R32 ();
	    ceiling->crush = R32 ();
	    ceiling->direction = R32 ();
	    ceiling->tag = R32 ();
	    ceiling->olddirection = R32 ();
	    ceiling->sector->specialdata = ceiling;

	    if (active)
		ceiling->thinker.function.acp1 = (actionf_p1)T_MoveCeiling;

	    P_AddThinker (&ceiling->thinker);
	    P_AddActiveCeiling(ceiling);
	    break;

	  case tc_door:
	    PADSAVEP();
	    door = Z_Malloc (sizeof(*door), PU_LEVEL, NULL);
	    memset (door, 0, sizeof (*door));
	    RThinker (&door->thinker);
	    door->type = R32 ();
	    door->sector = RSEC ();
	    door->topheight = R32 ();
	    door->speed = R32 ();
	    door->direction = R32 ();
	    door->topwait = R32 ();
	    door->topcountdown = R32 ();
	    door->sector->specialdata = door;
	    door->thinker.function.acp1 = (actionf_p1)T_VerticalDoor;
	    P_AddThinker (&door->thinker);
	    break;

	  case tc_floor:
	    PADSAVEP();
	    floor = Z_Malloc (sizeof(*floor), PU_LEVEL, NULL);
	    memset (floor, 0, sizeof (*floor));
	    RThinker (&floor->thinker);
	    floor->type = R32 ();
	    floor->crush = R32 ();
	    floor->sector = RSEC ();
	    floor->direction = R32 ();
	    floor->newspecial = R32 ();
	    floor->texture = R16 ();
	    R16 ();			// padding
	    floor->floordestheight = R32 ();
	    floor->speed = R32 ();
	    floor->sector->specialdata = floor;
	    floor->thinker.function.acp1 = (actionf_p1)T_MoveFloor;
	    P_AddThinker (&floor->thinker);
	    break;

	  case tc_plat:
	    PADSAVEP();
	    plat = Z_Malloc (sizeof(*plat), PU_LEVEL, NULL);
	    memset (plat, 0, sizeof (*plat));
	    active = RThinker (&plat->thinker);
	    plat->sector = RSEC ();
	    plat->speed = R32 ();
	    plat->low = R32 ();
	    plat->high = R32 ();
	    plat->wait = R32 ();
	    plat->count = R32 ();
	    plat->status = R32 ();
	    plat->oldstatus = R32 ();
	    plat->crush = R32 ();
	    plat->tag = R32 ();
	    plat->type = R32 ();
	    plat->sector->specialdata = plat;

	    if (active)
		plat->thinker.function.acp1 = (actionf_p1)T_PlatRaise;

	    P_AddThinker (&plat->thinker);
	    P_AddActivePlat(plat);
	    break;

	  case tc_flash:
	    PADSAVEP();
	    flash = Z_Malloc (sizeof(*flash), PU_LEVEL, NULL);
	    memset (flash, 0, sizeof (*flash));
	    RThinker (&flash->thinker);
	    flash->sector = RSEC ();
	    flash->count = R32 ();
	    flash->maxlight = R32 ();
	    flash->minlight = R32 ();
	    flash->maxtime = R32 ();
	    flash->mintime = R32 ();
	    flash->thinker.function.acp1 = (actionf_p1)T_LightFlash;
	    P_AddThinker (&flash->thinker);
	    break;

	  case tc_strobe:
	    PADSAVEP();
	    strobe = Z_Malloc (sizeof(*strobe), PU_LEVEL, NULL);
	    memset (strobe, 0, sizeof (*strobe));
	    RThinker (&strobe->thinker);
	    strobe->sector = RSEC ();
	    strobe->count = R32 ();
	    strobe->minlight = R32 ();
	    strobe->maxlight = R32 ();
	    strobe->darktime = R32 ();
	    strobe->brighttime = R32 ();
	    strobe->thinker.function.acp1 = (actionf_p1)T_StrobeFlash;
	    P_AddThinker (&strobe->thinker);
	    break;

	  case tc_glow:
	    PADSAVEP();
	    glow = Z_Malloc (sizeof(*glow), PU_LEVEL, NULL);
	    memset (glow, 0, sizeof (*glow));
	    RThinker (&glow->thinker);
	    glow->sector = RSEC ();
	    glow->minlight = R32 ();
	    glow->maxlight = R32 ();
	    glow->direction = R32 ();
	    glow->thinker.function.acp1 = (actionf_p1)T_Glow;
	    P_AddThinker (&glow->thinker);
	    break;

	  default:
	    I_Error ("P_UnarchiveSpecials:Unknown tclass %i "
		     "in savegame",tclass);
	}

    }

}

