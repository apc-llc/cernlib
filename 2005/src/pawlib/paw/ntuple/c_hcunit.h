/*
 *  c_hcunit.h  --
 *	Map the /HCUNIT/ common
 *
 *  Original: 19-Mar-1996 12:09
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcunit.h,v 1.3 1999/06/28 15:08:27 couet Exp $
 *
 *  $Log: c_hcunit.h,v $
 *  Revision 1.3  1999/06/28 15:08:27  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:48  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCUNIT
#define CERN_HCUNIT

#include	<cfortran/cfortran.h>


typedef struct {
	int	lout;
	int	lerr;
	int	linfit;
} hcunit_def;

#define HCUNIT	COMMON_BLOCK(HCUNIT,hcunit)
COMMON_BLOCK_DEF(hcunit_def,HCUNIT);

#endif	/*	CERN_HCUNIT	*/
