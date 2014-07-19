/*
 *  qp_compile.h  --
 *
 *  Original:  4-Jan-1995 18:51
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_compile.h,v 1.4 1996/04/23 18:38:11 maartenb Exp $
 *
 *  $Log: qp_compile.h,v $
 *  Revision 1.4  1996/04/23 18:38:11  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_COMPILE
#define CERN_QP_COMPILE

#include	"qp_query.h"


extern QuerySrc	*	the_qs_record;
extern int	 	qp_current_cut;


QueryExe *
qp_compile( QuerySrc * qs, bool match_expr, int * errp );

pTree
qp_get_cut_tree( int idx );

int
qp_parse_cut( int cid, int *errp );

#endif	/*	CERN_QP_COMPILE	*/
