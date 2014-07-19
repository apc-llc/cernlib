/*
 *  qp_generate.h  --
 *	Declarations for the generation of QP assembly.
 *
 *  Original: 24-Nov-1994 10:45
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_generate.h,v 1.2 1996/04/23 18:38:42 maartenb Exp $
 *
 *  $Log: qp_generate.h,v $
 *  Revision 1.2  1996/04/23 18:38:42  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_GENERATE
#define CERN_QP_GENERATE


QueryExe *
qp_generate( QueryTree *qtp, int * errp );

#endif	/*	CERN_QP_GENERATE	*/
