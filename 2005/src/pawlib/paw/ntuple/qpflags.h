/*
 *  qpflags.h  --
 *	Manage debug flags for the QP package
 *
 *  Original: 25-Jan-1995 11:29
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qpflags.h,v 1.2 1996/04/23 18:39:09 maartenb Exp $
 *
 *  $Log: qpflags.h,v $
 *  Revision 1.2  1996/04/23 18:39:09  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QPFLAGS
#define CERN_QPFLAGS


int
qp_flags_set( char *option, int value );

int
qp_flags_get( char *option );

void
qptime( int flag );

void
qpflags_();


#endif	/*	CERN_QPFLAGS	*/
