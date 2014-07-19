/*
 *  qp_symtab.c  --
 *	Implement the symbol table.
 *
 *  Original: 19-Oct-1994 16:00
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_symtab.c,v 1.9 1996/05/14 12:23:33 maartenb Exp $
 *
 *  $Log: qp_symtab.c,v $
 *  Revision 1.9  1996/05/14 12:23:33  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.8  1996/04/23 18:39:03  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>

#include	"errors.h"
#include	"qpflags.h"
#include	"qp_report.h"
#include	"qp_symtab.h"
#include	"str.h"
#include	"kuip_interface.h"


SymTab	*sf_symtab;


static unsigned int
hash_val( char * const name )
{
	register char		*p;
	register unsigned	h = 0, g, c;

	for( p = name; *p != 0 ; p++ ) {
		if ( isupper(*p) )
			c = tolower( *p );
		else
			c = *p;
		h = (h << 4) + c;
		if ( g = (h & 0xf0000000) ) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	return h;
}


SymTab *
st_new( unsigned int size )
{
	SymTab		*st;
	unsigned int	hsize;

	hsize =  4003;	/* should store list of primes and select */

	st = (SymTab *) calloc( sizeof( SymTab ), 1 );
	if ( st == 0 ) {
		return 0;
	}

	st->hsize = hsize;
	st->htab = (STEntry **) calloc( sizeof( STEntry * ), hsize );
	if ( st->htab == 0 ) {
		free( (void *) st );
		return 0;
	}

	st->size = size;
	st->cnt = 0;
	st->mark = 0;
	st->tab = (STEntry *) calloc( sizeof( STEntry ), size );
	if ( st->tab == 0 ) {
		free( (void *) st->htab );
		free( (void *) st );
		return 0;
	}

	return st;
}


STEntry *
st_entry( SymTab * st, STIndex index )
{
	qp_assert( index >= 0 && index < st->cnt );
	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_entry idx %d -> %s\n", index, st->tab[index].name );
	}
	return &st->tab[index];
}


STEntry *
st_lookup( SymTab * st, char * const name )
{
	STEntry	*e;
	STIndex idx;

	e = st->htab[ hash_val(name) % st->hsize ];

	while( e != 0 ) {
		if ( strcasecmp( name, e->name ) == 0 )
			break;
		e = e->next;
	}

	idx =  e != 0 ? e - st->tab : STI_INVALID ;
	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_lookup %s -> %d\n", name, idx );
	}
	return e;
}


STIndex
st_index( SymTab * st, char * const name )
{
	STEntry	*e;
	STIndex	idx;

	e = st->htab[ hash_val(name) % st->hsize ];

	while( e != 0 ) {
		if ( strcasecmp( name, e->name ) == 0 )
			break;
		e = e->next;
	}

	idx =  e != 0 ? e - st->tab : STI_INVALID ;
	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_index %s -> %d\n", name, idx );
	}
	return idx;
}


static int
insert( SymTab * st, char * const name, Signature * const s )
{
	STEntry	*e;
	unsigned int	i;

	qp_assert( st_lookup( st, name ) == 0 );

	if ( st->cnt == st->size ) {
		return R_ALLOC_ERROR;
	}

	e = &st->tab[st->cnt++];

	e->name = str_new( name );
	e->sig_list = s;
	e->sig_list->next = 0;
	i = hash_val(name) % st->hsize;

	e->next = st->htab[i];
	st->htab[i] = e;

	return R_NOERR;
}


int
st_insert( SymTab * st, char * const name, Signature * const s )
{
	STEntry	*e;
	int	r;

	e = st_lookup( st, name );

	if ( e != 0 ) {
		s->next = e->sig_list;
		e->sig_list = s;
		r = R_NOERR;
	} else {
		r = insert( st, name, s );
	}

	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_insert %s\n", name );
	}

	return r;
}


void
st_del( SymTab * st, char * const name )
{
	STEntry		*e, *e2;
	Signature	*sig;
	unsigned int	i;

	i = hash_val(name) % st->hsize;
	e = st->htab[ i ];

	if ( e == 0 ) {
		/* not in table ?? */
	} else if ( strcasecmp( name, e->name ) == 0 ) {
		st->htab[ i ] = e->next;
		str_del( e->name );
		e->name = 0;
		while ( e->sig_list != 0 ) {
			sig = e->sig_list;
			e->sig_list = sig->next;
			sig_del( sig );
		}
	} else {
		while( e->next != 0 ) {
			if ( strcasecmp( name, e->next->name ) == 0 ) {
				e2 = e->next;
				e->next = e2->next;
				str_del( e2->name );
				while ( e2->sig_list != 0 ) {
					sig = e2->sig_list;
					e2->sig_list = sig->next;
					sig_del( sig );
				}
				e2->name = 0;
				break;
			}
			e = e->next;
		}
	}
}


void
st_mark( SymTab * st )
{
	st->mark = st->cnt;
	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_mark\n" );
	}
}


void
st_sweep( SymTab * st )
{
	for ( ; st->cnt > st->mark ; ) {
		st_del( st, st->tab[st->cnt-1].name );
		st->cnt -= 1;
	}
	if ( qp_flags_get( "symtab" ) != 0 ) {
		printf( "st_sweep\n" );
	}
}
