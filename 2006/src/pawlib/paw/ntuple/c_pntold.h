/*
 *  pntold.h  --
 *	Map the /PNTOLD/ common
 *
 *  Original:  3-Apr-1995 11:53
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pntold.h,v 1.3 1999/06/28 15:08:36 couet Exp $
 *
 *  $Log: c_pntold.h,v $
 *  Revision 1.3  1999/06/28 15:08:36  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:53  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_PNTOLD
#define CERN_PNTOLD

#include	<cfortran/cfortran.h>

typedef struct {
	int	ntold;
} pntold_def;

#define PNTOLD COMMON_BLOCK(PNTOLD,pntold)
COMMON_BLOCK_DEF(pntold_def,PNTOLD);


#endif	/*	CERN_PNTOLD	*/
