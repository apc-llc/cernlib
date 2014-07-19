/*
 *  hcbits.h  --
 *	Map the /HCBITS/ common
 *
 *  Original:  3-Apr-1995 11:15
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcbits.h,v 1.3 1999/06/28 15:08:21 couet Exp $
 *
 *  $Log: c_hcbits.h,v $
 *  Revision 1.3  1999/06/28 15:08:21  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:41  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCBITS
#define CERN_HCBITS

#include        <cfortran/cfortran.h>


typedef struct {
	int	i1;
	int	i2;
	int	i3;
	int	i4;
	int	i5;
	int	i6;
	int	i7;
	int	i8;
	int	i9;
	int	i10;
	int	i11;
	int	i12;
	int	i13;
	int	i14;
	int	i15;
	int	i16;
	int	i17;
	int	i18;
	int	i19;
	int	i20;
	int	i21;
	int	i22;
	int	i23;
	int	i24;
	int	i25;
	int	i26;
	int	i27;
	int	i28;
	int	i29;
	int	i30;
	int	i31;
	int	i32;
	int	i33;
	int	i34;
	int	i35;
	int	i123;
	int	i230;
} hcbits_def;

#define HCBITS COMMON_BLOCK(HCBITS,hcbits)
COMMON_BLOCK_DEF(hcbits_def,HCBITS);

#endif	/*	CERN_HCBITS	*/
