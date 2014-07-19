/*
 *  pawidn.h  --
 *	Map the /PAWIDN/ common block
 *
 *  Original: 25-Aug-1995 14:23
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pawidn.h,v 1.3 1999/06/28 15:08:31 couet Exp $
 *
 *  $Log: c_pawidn.h,v $
 *  Revision 1.3  1999/06/28 15:08:31  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:50  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<cfortran/cfortran.h>


#ifndef CERN_PAWIDN
#define CERN_PAWIDN


typedef struct {
	int		idnevt;
	float		vidn1, vidn2, vidn3, vidn[10];
	float		x[512];
} pawidn_def;

#define PAWIDN COMMON_BLOCK(PAWIDN,pawidn)

COMMON_BLOCK_DEF(pawidn_def,PAWIDN);

#endif	/*	CERN_PAWIDN	*/
