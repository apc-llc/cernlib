/*
 *  qp_dyn_check.h  --
 *	Mark the nodes in the syntax tree for runtime checking of 
 *	vector/array/... checking.
 *
 *  Original: 12-Apr-1995 19:53
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_dyn_check.h,v 1.4 1996/04/23 18:38:17 maartenb Exp $
 *
 *  $Log: qp_dyn_check.h,v $
 *  Revision 1.4  1996/04/23 18:38:17  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_DYN_CHECK
#define CERN_QP_DYN_CHECK

#include	"cern_types.h"
#include	"qp_query.h"


void
qp_dyn_check( QueryTree *qt );

void
qp_dyn_list( bool need_shape, int n, pTree tl[], int * ncheck );

#endif	/*	CERN_QP_DYN_CHECK	*/
