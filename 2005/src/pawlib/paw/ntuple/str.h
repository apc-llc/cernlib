/*
 *  str.h  --
 *	declare str routines
 *
 *  Original: 22-Jun-1994 00:52
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: str.h,v 1.8 1996/04/23 18:39:12 maartenb Exp $
 *
 *  $Log: str.h,v $
 *  Revision 1.8  1996/04/23 18:39:12  maartenb
 *  - Add RCS keywords
 *
 *
 *  $Id: str.h,v 1.8 1996/04/23 18:39:12 maartenb Exp $
 *
 *  $Log: str.h,v $
 *  Revision 1.8  1996/04/23 18:39:12  maartenb
 *  - Add RCS keywords
 *
 *  Revision 1.7  1996/04/23 18:18:51  maartenb
 *  - Add proper multiple include guard.
 *
 *  Revision 1.6  1996/04/23 18:17:00  maartenb
 *  - Typographical fixes
 *
 *  - RCS keywords
 *
 *
 */

#ifndef CERN_STR
#define CERN_STR

#include	<stdarg.h>


char *
str_new( const char * const s );

char *
str_alloc( const int len );

char *
str_merge( char * s, ... );

char *
str_vmerge( char * s, va_list ap );

void
str_del( char * s );

char *
str_toupper( const char * const s );

char *
str_tolower( const char * const s );

int
str_atoi( const char * const s, const int base, int * valp );

int
str_atol( const char * const s, const int base, long * valp );

char *
str_subrange( const char * const s, int from, int len );

#endif	/*	CERN_STR	*/
