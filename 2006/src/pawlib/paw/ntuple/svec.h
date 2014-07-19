/*
 *  svec.h  --
 *	Implement a vector of strings ADT
 *
 *  Original:  6-Dec-1995 23:09
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: svec.h,v 1.4 1996/04/23 18:39:13 maartenb Exp $
 *
 *  $Log: svec.h,v $
 *  Revision 1.4  1996/04/23 18:39:13  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_SVEC
#define CERN_SVEC

#include	"cern_types.h"


typedef struct _svec_struct_ {
	int		fSize;
	int		fEntries;
	String *	fV;
} SVecStruct;

typedef SVecStruct	*SVec;


extern SVec
svec_new( int n );

extern SVec
svec_copy( SVec v );

extern void
svec_add( SVec v, String s );

extern String
svec_get( SVec v, const int i );

extern void
svec_del( SVec v );

extern int
svec_entries( SVec v );

#endif	/*	CERN_SVEC	*/
