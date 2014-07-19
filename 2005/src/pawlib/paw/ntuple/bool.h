/*
 *  bool.h  --
 *
 *  Original: 23-Apr-1995 19:59
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: bool.h,v 1.2 1996/04/23 18:37:11 maartenb Exp $
 *
 *  $Log: bool.h,v $
 *  Revision 1.2  1996/04/23 18:37:11  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_BOOL
#define CERN_BOOL



#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE (! FALSE)
#endif

typedef int bool;

#endif	/*	CERN_BOOL	*/
