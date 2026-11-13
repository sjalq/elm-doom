// DOOM-ORACLE changes to this linuxdoom-1.10 file (doom/oracle; search
// for DOOM-ORACLE below). None of them changes how the game plays:
//  - ORACLE_LUMPPAD: zero bytes allocated past every cached lump.

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
// DESCRIPTION:
//	WAD I/O functions.
//
//-----------------------------------------------------------------------------


#ifndef __W_WAD__
#define __W_WAD__


#ifdef __GNUG__
#pragma interface
#endif


//
// TYPES
//
typedef struct
{
    // Should be "IWAD" or "PWAD".
    char		identification[4];		
    int			numlumps;
    int			infotableofs;
    
} wadinfo_t;


typedef struct
{
    int			filepos;
    int			size;
    char		name[8];
    
} filelump_t;

//
// WADFILE I/O related stuff.
//
typedef struct
{
    char	name[8];
    int		handle;
    int		position;
    int		size;
} lumpinfo_t;


// DOOM-ORACLE: R_DrawColumn indexes a column with (frac>>FRACBITS)&127,
// so a sprite post or a texture column shorter than 128 can be read up to
// 127 bytes past its end, i.e. past the end of the lump (or composite
// texture) holding it. The original then reads whatever the heap holds
// next (another zone block, its header): those pixels depend on memory
// layout. The oracle gives every cached lump and composite texture this
// many zero bytes past its end, so such reads see 0, which a port can
// reproduce: bytes past the end of a lump read as 0.
#define ORACLE_LUMPPAD	256

extern	void**		lumpcache;
extern	lumpinfo_t*	lumpinfo;
extern	int		numlumps;

void    W_InitMultipleFiles (char** filenames);
void    W_Reload (void);

int	W_CheckNumForName (char* name);
int	W_GetNumForName (char* name);

int	W_LumpLength (int lump);
void    W_ReadLump (int lump, void *dest);

void*	W_CacheLumpNum (int lump, int tag);
void*	W_CacheLumpName (char* name, int tag);




#endif
//-----------------------------------------------------------------------------
//
// $Log:$
//
//-----------------------------------------------------------------------------
