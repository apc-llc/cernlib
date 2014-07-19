/*
 *  hcntpaw.h  --
 *	Map the /PAWCR8/, /PAWCR4/, and /PAWC32/ commons
 *
 *  Original: 18-Jan-1995 23:08
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcntpaw.h,v 1.3 1999/06/28 15:08:25 couet Exp $
 *
 *  $Log: c_hcntpaw.h,v $
 *  Revision 1.3  1999/06/28 15:08:25  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:46  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCNTPAW
#define CERN_HCNTPAW

#include	<cfortran/cfortran.h>


typedef struct {
	double	rvar8[1000];
} pawcr8_def;

#define PAWCR8 COMMON_BLOCK(PAWCR8,pawcr8)
COMMON_BLOCK_DEF(pawcr8_def,PAWCR8);


typedef struct {
	float	rvar4[50007];
} pawcr4_def;

#define PAWCR4 COMMON_BLOCK(PAWCR4,pawcr4)
COMMON_BLOCK_DEF(pawcr4_def,PAWCR4);


typedef struct {
	char	cvar32[1000][32];
} pawc32_def;

#define PAWC32 COMMON_BLOCK(PAWC32,pawc32)
COMMON_BLOCK_DEF(pawc32_def,PAWC32);


#endif	/*	CERN_HCNTPAW	*/
