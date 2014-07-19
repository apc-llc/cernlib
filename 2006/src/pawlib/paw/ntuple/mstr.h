/*
 *  mstr.h  -  Declare 'marked' string routines
 *	       used by the parser.
 *
 *  Original: 22-Jun-1994 00:52
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: mstr.h,v 1.2 1996/04/23 18:38:06 maartenb Exp $
 *
 *  $Log: mstr.h,v $
 *  Revision 1.2  1996/04/23 18:38:06  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef _mstr_h
#define _mstr_h

char *
mstr_new( char *const s );

char *
mstr_merge( char *s, ... );

void
mstr_del( char *const s );

void
mstr_mark( char *const s );

void
mstr_release( char *const s );

void
mstr_del_all( void );

void
mstr_reset( void );

#endif	/*	_mstr_h	*/
