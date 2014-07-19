/*
 *  c_pcaddr.h  --
 *	Map the /PCADDR/ common
 *
 *  Original: 11-Dec-1995 18:13
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pcaddr.h,v 1.3 1999/06/28 15:08:31 couet Exp $
 *
 *  $Log: c_pcaddr.h,v $
 *  Revision 1.3  1999/06/28 15:08:31  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.2  1996/04/23 18:37:51  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_C_PCADDR
#define CERN_C_PCADDR

#include	<cfortran/cfortran.h>


typedef struct {
	int	jmpant;
	int	jmlab;
	int	jmrow;
	int	jmchain;
	int	jntwini;
	int	jntwchk;
	int	jmhist;
} pcaddr_def; 


#define PCADDR COMMON_BLOCK(PCADDR,pcaddr)
COMMON_BLOCK_DEF(pcaddr_def,PCADDR);

#endif	/*	CERN_C_PCADDR	*/
