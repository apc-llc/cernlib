/*
 *  mstr.c  -  Marked pool of strings using dynamic string package
 *
 *  Original: 22-Jun-1994 00:52
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: mstr.c,v 1.4 1996/04/23 18:38:05 maartenb Exp $
 *
 *  $Log: mstr.c,v $
 *  Revision 1.4  1996/04/23 18:38:05  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>
#include	<stdarg.h>
#include	<assert.h>
#include	<string.h>
#include	<stdio.h>

#include	"str.h"
#include	"mstr.h"


char *
mstr_new( char * const s )
{
	char		*st;

	st = str_new( s );

	mstr_mark( st );

	return st;
}


char *
mstr_merge( char * s, ... )
{
	va_list		ap;
	char		*st;

	mstr_release( s );	/* release now and mark later */
				/* because realloc can change pointer */

	va_start( ap, s );
	while ( (st = va_arg( ap, char * )) != 0 ) {
		mstr_release( st );
	}
	va_end( ap );
	
	va_start( ap, s );
	s = str_vmerge( s, ap );
	va_end( ap );

	mstr_mark( s );

	return s;
}


void
mstr_del( char *const s )
{
	mstr_release( s );
	str_del( s );
}


/*
 *  Hash package to track allocated strings until 
 *  they are part of the syntax tree. 
 *  A double hashing algorithm is used
 */


#define	HASH_TABLE_SIZE		503	/* should be prime (1009,2003,4003) */

#define HASH_FREE	((char *) 0)
#define HASH_DELETED	((char *) 1)


static char	*mark_tab[HASH_TABLE_SIZE];
static int	tab_cnt;
int	tab_max;


static int
str_h1( char *const s )
{
	return (long) s % HASH_TABLE_SIZE;
}


static int
str_h2( char *const s )
{
	int	u;
	u = ( HASH_TABLE_SIZE - 2 - (long) s ) % ( HASH_TABLE_SIZE - 2 );

	if ( u == 0 ) u = 1; 
	return u;
}


void
mstr_mark( char *const s )
{
	int	i, j, n;

	if ( tab_cnt >= HASH_TABLE_SIZE ) {
		assert(0);
	}

	i = str_h1( s );
	j = str_h2( s );

	for( n=0; n < HASH_TABLE_SIZE ; n++ ) {
		if ( mark_tab[i] == HASH_FREE || mark_tab[i] == HASH_DELETED ) {
			mark_tab[i] = s;
			tab_cnt++;
			if ( tab_cnt > tab_max ) tab_max = tab_cnt;
			break;
		}
		i = ( i + j ) % HASH_TABLE_SIZE;
	}

	assert( n < HASH_TABLE_SIZE );
}


void
mstr_release( char *const s )
{
	int	i, j, n;

	i = str_h1( s );
	j = str_h2( s );

	for( n=0; n < HASH_TABLE_SIZE ; n++ ) {
		if ( mark_tab[i] == s) {
			mark_tab[i] = HASH_DELETED;
			tab_cnt--;
			break;
		}
		if ( mark_tab[i] == HASH_FREE ) {
			assert(0);
		}
		i = ( i + j ) % HASH_TABLE_SIZE;
	}
}


void
mstr_del_all( void )
{
	int	n;
	char	**p;

	for( n=0, p=mark_tab; n < HASH_TABLE_SIZE ; n++, p++ ) {
		if ( *p != HASH_FREE && *p != HASH_DELETED ) {
			free( *p );
		}
	}
}


void
mstr_reset( void )
{
	int	n;
	char	**p;

	for( n=0, p=mark_tab; n < HASH_TABLE_SIZE ; n++ ) {
		*p++ = HASH_FREE;
	}
	tab_cnt = 0;
}
