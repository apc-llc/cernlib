/*
 *  qp_vvec.c  --
 *	Implement Variable and VarVec.
 *
 *  Original: 10-May-1996 16:10
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_vvec.c,v 1.1 1996/05/12 15:13:56 maartenb Exp $
 *
 *  $Log: qp_vvec.c,v $
 *  Revision 1.1  1996/05/12 15:13:56  maartenb
 *  - Replaced the handling of variables. The handling of variables is now
 *  fully dynamic and there is no other limit than the commonblocks to the
 *  number of var's in a query.
 *
 *
 *
 */

#include	<stdlib.h>

#include	"qp_report.h"
#include	"qp_vvec.h"


Variable
var_alloc()
{
	Variable	v;

	v = (Variable) calloc( 1, sizeof(VarStruct) ); qp_assert( v != 0 );

	return v;
}


Variable
var_new( Variable oldv )
{
	Variable	newv;

	newv = var_alloc();

	memcpy( newv, oldv, sizeof(VarStruct) );

	return newv;
}


void
var_del( Variable v )
{
	free( (void *) v );
}


VVec
vvec_new( int n )
{
	VVec	vv;

	if ( n <= 0 ) {
		n = 1;	/* some mallocs return NULL when allocating 0 bytes :-) */
	}

	vv = (VVec) calloc( sizeof(VVecStruct), 1 );
	qp_assert( vv != 0 );

	vv->fSize = n;
	vv->fV = (Variable *) calloc( sizeof(char *), vv->fSize );
	qp_assert( vv->fV != 0 );

	return vv;
}


VVec
vvec_copy( VVec ov )
{
	int		i;
	VVec		nv;

	nv = vvec_new( ov->fSize );

	for ( i=0 ; i < ov->fEntries ; i++ ) {
		vvec_add( nv, var_new(ov->fV[i]) );
	}

	return nv;
}


void
vvec_add( VVec vv, Variable v )
{
	if ( vv->fEntries == vv->fSize ) {
		vv->fSize *= 2;
		vv->fV = (Variable *) realloc( vv->fV, sizeof(char *)*vv->fSize );
		qp_assert( vv->fV != 0 );
	}

	vv->fV[ vv->fEntries ] = v;
	vv->fEntries += 1;
}


Variable
vvec_get( VVec vv, const int i )
{
	qp_assert( (i >= 0) && (i < vv->fEntries) );

	return vv->fV[i];
}


void
vvec_del( VVec vv )
{
	int	i;

	for ( i=0 ; i < vv->fEntries ; i++ ) {
		var_del( vv->fV[i] );
	}

	free( (void *) vv->fV );
	free( (void *) vv );

}


int
vvec_entries( VVec vv )
{
	return vv->fEntries;
}
