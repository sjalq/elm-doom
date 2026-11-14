// DOOM-ORACLE changes to this linuxdoom-1.10 file (verify/oracle; search
// for DOOM-ORACLE below). None of them changes how the game plays:
//  - finetangent and finesine are the two halves of one array,
//    oracle_finetrig, finetangent first (see tables.h).

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
//	Lookup tables.
//	Do not try to look them up :-).
//	In the order of appearance: 
//
//	int finetangent[4096]	- Tangens LUT.
//	 Should work with BAM fairly well (12 of 16bit,
//      effectively, by shifting).
//
//	int finesine[10240]		- Sine lookup.
//	 Guess what, serves as cosine, too.
//	 Remarkable thing is, how to use BAMs with this? 
//
//	int tantoangle[2049]	- ArcTan LUT,
//	  maps tan(angle) to angle fast. Gotta search.	
//    
//-----------------------------------------------------------------------------


#ifndef __TABLES__
#define __TABLES__



#ifdef LINUX
#include <math.h>
#else
#define PI				3.141592657
#endif


#include "m_fixed.h"
	
#define FINEANGLES		8192
#define FINEMASK		(FINEANGLES-1)


// 0x100000000 to 0x2000
#define ANGLETOFINESHIFT	19		

// DOOM-ORACLE: finetangent[4096] and finesine[10240] are one array,
// finetangent first. That is how they lie in memory in the original
// binary (tables.c defines them in that order), and the game relies on
// it: R_ScaleFromGlobalAngle indexes finesine with a signed int shifted
// right, i.e. with -4096..-1 for angles past ANG180, which reads
// finetangent[0..4095]. A compiler is free to place two separate arrays
// apart (AddressSanitizer does); one array keeps the original values.
extern  fixed_t		oracle_finetrig[FINEANGLES/2 + 5*FINEANGLES/4];
#define finetangent	(oracle_finetrig)

// Effective size is 10240.
#define finesine	(oracle_finetrig + FINEANGLES/2)

// Re-use data, is just PI/2 pahse shift.
extern  fixed_t*	finecosine;


// Effective size is 4096.
// DOOM-ORACLE: finetangent is defined above

// Binary Angle Measument, BAM.
#define ANG45			0x20000000
#define ANG90			0x40000000
#define ANG180		0x80000000
#define ANG270		0xc0000000


#define SLOPERANGE		2048
#define SLOPEBITS		11
#define DBITS			(FRACBITS-SLOPEBITS)

typedef unsigned angle_t;


// Effective size is 2049;
// The +1 size is to handle the case when x==y
//  without additional checking.
extern angle_t		tantoangle[SLOPERANGE+1];


// Utility function,
//  called by R_PointToAngle.
int
SlopeDiv
( unsigned	num,
  unsigned	den);


#endif
//-----------------------------------------------------------------------------
//
// $Log:$
//
//-----------------------------------------------------------------------------
