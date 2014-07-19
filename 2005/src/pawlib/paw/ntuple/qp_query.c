/*
 *  qp_query.c  --
 *	Implement the allocation and release of the various
 *	QueryXXX structures
 *
 *  Original:  4-Jan-1995 18:47
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_query.c,v 1.13 1996/08/27 10:13:54 lecointe Exp $
 *
 *  $Log: qp_query.c,v $
 *  Revision 1.13  1996/08/27 10:13:54  lecointe
 *  Added a call to a procedure that free one of the field of the QueryExe struct
 *
 *  Revision 1.12  1996/04/23 18:38:53  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<assert.h>
#include	<stdlib.h>
#include	<string.h>

#include	"errors.h"
#include	"qp_query.h"
#include	"qp_report.h"
#include	"str.h"
#include	"qp_vvec.h"


QuerySrc *
qp_qs_new(
	char *		path,
	int		id,
	String		select,
	SVec		expressions
)
{
	int		i, n;
	QuerySrc *	qs;

	qs = (QuerySrc *) calloc( sizeof(QuerySrc), 1 );
	if ( qs == 0 ) {
		qp_abort( "qp_qs_new: Allocation error\n" );
	}

	qs->path = str_new( path );
	qs->id = id;
	if ( (select != 0) && (strlen(select) > 0) ) {
		qs->hasSel = TRUE;
		qs->sel = str_new( select );
	} else {
		qs->hasSel = FALSE;
	}

	n = svec_entries( expressions );
	if ( n > MAX_EXPRS ) {
		sf_report( "Only showing first %d of expressions\n", MAX_EXPRS, n );
		n = MAX_EXPRS;
	}

	for( i=0 ; i < n ; i++ ) {
		qs->expr[i] = str_new( svec_get(expressions,i) );
	}
	qs->nexpr = n;

	return qs;
}


void
qp_qs_free( QuerySrc * qs )
{
	int	i;

	free( (void *) qs->path );

	if ( qs->hasSel )
		free( (void *) qs->sel );
	
	for( i=0 ; i < qs->nexpr ; i++ )
		free( (void *) qs->expr[i] );
	
	free( (void *) qs );
}


QueryTree *
qp_qt_new( char * caller, int * errp )
{
	QueryTree *	qt;

	qt = (QueryTree *) calloc( sizeof(QueryTree), 1 );
	if ( qt == 0 ) {
		sf_report( "%s: Cannot allocate QueryTree structure\n",
			caller );
		*errp = R_ALLOC_ERROR;
	}

	return qt;
}


void
qp_qt_free( QueryTree * qt )
{
	int	i;

	assert( qt != 0 );

	free( (void *) qt->path );

	if ( qt->hasSel )
		free_tree( qt->sel );
	
	for( i=0 ; i < qt->nexpr ; i++ )
		free_tree( qt->expr[i] );
	
	for( i=0 ; i < qt->ncuts ; i++ )	/* check because of recursive */
		if ( qt->cuts[i] != 0 ) {	/* cut parsing */
			free_tree( qt->cuts[i] );
		}

	free( (void *) qt );
}


QueryExe *
qp_qe_new( char * caller, int * errp )
{
	QueryExe *	qe;

	qe = (QueryExe *) calloc( sizeof(QueryExe), 1 );
	if ( qe == 0 ) {
		sf_report( "%s: Cannot allocate QueryExe structure\n",
			caller );
		*errp = R_ALLOC_ERROR;
	}

	qe->mask_write = -1;

	return qe;
}


void
qp_qe_free( QueryExe * qe )
{
	int	i;

	assert( qe != 0 );

	free( (void *) qe->path );

	if ( qe->hasSel ) {
		free( (void *) qe->sel );
		str_del( qe->sel_str );
	}
	
	for( i=0 ; i < qe->nexpr ; i++ ) {
		free( (void *) qe->expr[i] );
		str_del( qe->expr_str[i] );
	}
	
	for( i=0 ; i < qe->ncuts ; i++ )
		free( (void *) qe->cuts[i] );

	vvec_del( qe->vv );

	free( (void *) qe );
}


bool
qp_has_string_expr( QueryExe * qe )
{
	int 	i;

	assert ( qe != 0 );

	for ( i=0 ; i < qe->nexpr ; i++ ) {	
		if ( qe->expr_type[i] == D_STR ) {
			return TRUE;
		}
	}

	return FALSE;
}
