/*
 *  pcmask.h  --
 *	Map the /PCMASK/ and /PCMAS2/ common blocks
 *
 *  Original: 11-Nov-1994 11:26
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pcmask.h,v 1.3 1999/06/28 15:08:34 couet Exp $
 *
 *  $Log: c_pcmask.h,v $
 *  Revision 1.3  1999/06/28 15:08:34  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:51  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_PCMASK
#define CERN_PCMASK

#include	<cfortran/cfortran.h>

#define		MASDIM	9

typedef struct {
	int	lmask;
	int	lmbuf;
	int	lm;
	int	imsk[MASDIM];
	int	imrd;
	int	mbit[MASDIM];
	int	mbitd;
	int	numd;
	int	nbuffd;
	int	nbuffr;
	int	nbuff[MASDIM];
	int	idec;
	int	lrec;
	int	nbmask;
} pcmask_def;

#define	PCMASK	COMMON_BLOCK(PCMASK,pcmask)

COMMON_BLOCK_DEF(pcmask_def,PCMASK);

typedef struct {
	char	chmask[20][32];
} pcmas2_def;

#define	PCMAS2	COMMON_BLOCK(PCMAS2,pcmas2)

COMMON_BLOCK_DEF(pcmas2_def,PCMAS2);

#endif	/*	CERN_PCMASK	*/
