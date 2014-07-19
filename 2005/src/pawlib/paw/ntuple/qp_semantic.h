/*
 *  qp_semantic.h  --
 *	Declaration of the entry point for the semantic analysis
 *
 *  Original: 13-Jul-1994 11:07
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_semantic.h,v 1.4 1996/04/23 18:39:00 maartenb Exp $
 *
 *  $Log: qp_semantic.h,v $
 *  Revision 1.4  1996/04/23 18:39:00  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_SEMANTIC
#define CERN_QP_SEMANTIC

#include	"qp_tree.h"

pTree
sem_check( pTree t, int *r );

void
qp_sem_cmd( pTree st, int n, pTree et[], int *r );

#endif	/*	CERN_QP_SEMANTIC	*/
