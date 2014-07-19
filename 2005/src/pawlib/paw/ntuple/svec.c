/*
 *  svec.c  --
 *	Implement the String vector ADT.
 *	The strings are owned by the SVec.
 *
 *  Original:  6-Dec-1995 23:42
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: svec.c,v 1.6 1996/04/23 18:39:12 maartenb Exp $
 *
 *  $Log: svec.c,v $
 *  Revision 1.6  1996/04/23 18:39:12  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>

#include	"qp_report.h"
#include	"str.h"
#include	"svec.h"


SVec
svec_new( int n )
{
	SVec	v;

	if ( n <= 0 ) {
		n = 1;	/* some mallocs return NULL when allocating 0 bytes :-) */
	}

	v = (SVec) calloc( sizeof(SVecStruct), 1 );
	qp_assert( v != 0 );

	v->fSize = n;
	v->fV = (String *) calloc( sizeof(char *), v->fSize );
	qp_assert( v->fV != 0 );

	return v;
}


SVec
svec_copy( SVec ov )
{
	int		i;
	SVec		nv;

	nv = svec_new( ov->fSize );

	for ( i=0 ; i < ov->fEntries ; i++ ) {
		svec_add( nv, str_new(ov->fV[i]) );
	}

	return nv;
}


void
svec_add( SVec v, String s )
{
	if ( v->fEntries == v->fSize ) {
		v->fSize *= 2;
		v->fV = (String *) realloc( v->fV, sizeof(char *)*v->fSize );
		qp_assert( v->fV != 0 );
	}

	v->fV[ v->fEntries ] = s;
	v->fEntries += 1;
}


String
svec_get( SVec v, const int i )
{
	qp_assert( (i >= 0) && (i < v->fEntries) );

	return v->fV[i];
}


void
svec_del( SVec v )
{
	int	i;

	for ( i=0 ; i < v->fEntries ; i++ ) {
		str_del( v->fV[i] );
	}

	free( (void *) v->fV );
	free( (void *) v );

}


int
svec_entries( SVec v )
{
	return v->fEntries;
}
