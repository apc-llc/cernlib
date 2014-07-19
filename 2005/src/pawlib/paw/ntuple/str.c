/*
 *  str.c  -  Dynamic string package
 *
 *  Original: 22-Jun-1994 00:52
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: str.c,v 1.9 1996/06/17 09:18:17 couet Exp $
 *
 *  $Log: str.c,v $
 *  Revision 1.9  1996/06/17 09:18:17  couet
 *  - The extraction of the MASK name from the file name was incorrect.
 *    This made impossible to use masks outside the current directory.
 *
 *  Revision 1.8  1996/04/23 18:39:11  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<assert.h>
#include	<ctype.h>
#include	<errno.h>
#include	<limits.h>
#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"str.h"


char *
str_new( const char * const s )
{
	char		*st;
	unsigned int	len;

	len = strlen( (char *) s ) + 1;
	st = calloc( 1, len ); assert( st );
	strcpy( st, (char *) s );

	return st;
}


char *
str_alloc( const int len )
{
	char		*st;

	st = calloc( 1, len ); assert( st );

	return st;
}


char *
str_merge( char * s, ... )
{
	va_list		ap;
	char		*st;
	unsigned int	len;

	va_start( ap, s );
	
	len = strlen( s ) + 1;
	while ( (st = va_arg( ap, char * )) != 0 ) {
		len += strlen( st );
		s = realloc( s, len ); assert( s );
		strcat( s, st );
		free( st );
	}

	va_end( ap );

	return s;
}


void
str_del( char * s )
{
	free( s );
}


char *
str_vmerge( char * s, va_list ap )
{
	char		*st;
	unsigned int	len;

	len = strlen( s ) + 1;
	while ( (st = va_arg( ap, char * )) != 0 ) {
		len += strlen( st );
		s = realloc( s, len ); assert( s );
		strcat( s, st );
		str_del( st );
	}

	return s;
}


char *
str_toupper( const char * const s )
{
	char *		new;
	char *		p;

	new = str_new( s );

	for ( p = new ; *p != '\0' ; p++ ) {
		*p = toupper( *p );
	}

	return new;
}


char *
str_tolower( const char * const s )
{
	char *		new;
	char *		p;

	new = str_new( s );

	for ( p = new ; *p != '\0' ; p++ ) {
		*p = tolower( *p );
	}

	return new;
}


int
str_atoi( const char * const s, const int base, int * valp )
{
	extern int	errno;
	char		*endp;
	long int	val;

	errno = 0;

	if ( *s == '\0' ) {
		return 1;
	}

	val = strtol( s, &endp, base );

	if ( errno != 0 ) {
		return 1;
	}

	if ( *endp != '\0' ) {
		return 1;
	}

	if ( (val > INT_MAX) || (val < INT_MIN) ) {
		errno = ERANGE;
		return 1;
	}

	*valp = val;


	return 0;
}


int
str_atol( const char * const s, const int base, long * valp )
{
	extern int	errno;
	char		*endp;
	long int	val;

	errno = 0;

	if ( *s == '\0' ) {
		return 1;
	}

	val = strtol( s, &endp, base );

	if ( errno != 0 ) {
		return 1;
	}

	if ( *endp != '\0' ) {
		return 1;
	}

	*valp = val;

	return 0;
}


char *
str_subrange( const char * const s, int from, int len )
{
	char		*st;
	unsigned int	len_s;

	len_s = strlen( (char *) s );
	len_s -= from;
	if ( len_s < len ) {
		len = len_s;
	}

	st = calloc( 1, len + 1 ); assert( s );
	strncpy( st, (char *) s, len );
	st[len] = '\0';

	return st;
}


char *
str_mask_name( const char * const s, int from, int iend )
{
	char *st;
	int  i,j,ifirst;

	st = calloc( 1, iend + 1 ); assert( s );

	if (from == 0 ) {
		ifirst = from;
	} else {
		ifirst = from+1;
	}

	j = 0;

	for (i=ifirst ; i<iend ; i++) {
		st[j] = s[i];
		j++;
	}
	st[j] = '\0';

	return st;
}
