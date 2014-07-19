/*
 *  hash_table.h  --
 *	Macro implementation of a self growing hash table.
 *	- Keys are strings 
 *	- Data is of type HashData ( should be a define of a typedef )
 *	Data is not owned by the table.
 *
 *  Original:  6-Dec-1995 15:28
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: hash_table.h,v 1.7 1996/05/14 12:23:23 maartenb Exp $
 *
 *  $Log: hash_table.h,v $
 *  Revision 1.7  1996/05/14 12:23:23  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.6  1996/04/23 18:37:58  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	"cern_types.h"
#include	"qp_report.h"
#include	"kuip_interface.h"


#define	KEY_FREE	((char *) 0)
#define	KEY_DELETED	((char *) 1)

#if !defined(HASH_PREF) || !defined(HashData)
#error	Please define HASH_PREF (A prefix for the datatype) and	\
	HashData the dataype associated with the key.
#endif

#define	_HC(a,b)	a ## b
#define	HC(a,b)		_HC(a,b)
#define	_ST(a)		#a
#define	ST(a)		_ST(a)


#define	HashSlot	HC(HASH_PREF,_slot)

typedef struct {
	char		*fKey;
	HashData	fData;
} HashSlot; 


#define	HashTable	HC(HASH_PREF,Table)
#define	HashTableStruct	HC(HASH_PREF,TableStruct)
typedef struct {
	int		fBits;
	int		fEntries;
	HashSlot *	fTab;
} HashTableStruct; 

typedef HashTableStruct	*HashTable;

#define	HashNew		HC(HASH_PREF,_new)
#define	HashGrow	HC(HASH_PREF,_grow)
#define	HashHash	HC(HASH_PREF,_hash)
#define	HashAdd		HC(HASH_PREF,_add)
#define	HashRemove	HC(HASH_PREF,_remove)
#define	HashFind	HC(HASH_PREF,_find)
#define	HashDel		HC(HASH_PREF,_del)



extern HashTable
HashNew( int n );

extern bool
HashGrow( HashTable t );

extern void
HashHash( HashTable t, const char * const key, int *h1, int *h2 );

extern bool
HashAdd( HashTable t, char * key, HashData d );

extern bool
HashRemove( HashTable t, char * key );

extern bool
HashFind( HashTable t, char * key, HashData * d );

extern void
HashDel( HashTable t );


#if defined(HASH_IMPLEMENT)

#include	<ctype.h>
#include	<stdlib.h>


HashTable
HashNew( int n )
{
	HashTable	t;
	int		bits, size;

	t = (HashTable) calloc( sizeof( HashTableStruct ), 1 );
	if ( t == 0 ) return 0;

	for( bits=2 ; (size = (1<<bits)) < n ; bits++ ) {}

	t->fTab = (HashSlot *) calloc( sizeof(HashSlot), size );
	if ( t->fTab == 0 ) {
		free( (void *) t );
		return 0;
	}

	t->fBits = bits;

	return t;
}


bool
HashGrow( HashTable t )
{
	HashSlot	*old_tab, *new_tab;
	int		old_size, new_size, i;

	old_size = 1 << (t->fBits);
	new_size = 1 << (t->fBits + 1);
	new_tab = (HashSlot *) calloc( sizeof(HashSlot), new_size );
	if ( new_tab == 0 ) {
		return FALSE;
	}

	old_tab = t->fTab;
	t->fTab = new_tab;
	t->fBits += 1;
	t->fEntries = 0;

	for ( i=0 ; i < old_size ; i++ ) {
		if ( old_tab[i].fKey != KEY_DELETED &&
			old_tab[i].fKey != KEY_FREE
		) {
			HashAdd( t, old_tab[i].fKey, old_tab[i].fData );
		}
	}

	free( (void *) old_tab );

	return TRUE;
}


void
HashHash(
	HashTable		t,
	const char * const	key,
	int *			h1,
	int *			h2
) {
	register const char	*p;
	register unsigned	h, g, c;
	int			u, m;

	h = 0;
	for( p = key; *p != 0 ; p++ ) {
		if ( isupper(*p) )
			c = tolower( *p );
		else
			c = *p;
		h = (h << 4) + c;
		if ( g = (h & 0xf0000000U) ) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	m =  ((1 << t->fBits ) - 1);

	u = ( ( m - 2 - h ) % ( m - 2 ) ) | 0x1; /* make the step odd */

	*h1 =  h & m;
	*h2 =  u;

}

bool
HashAdd(
	HashTable	t,
	char *		key,
	HashData	d
) {
	int	i, step, j, m;

	m =  1 << t->fBits;

	if ( t->fEntries > 0.75 * m ) {		/* auto grow */
		if ( ! HashGrow( t ) )
			return FALSE;
	}

	HashHash( t, key, &i, &step );
	for( j=0; j < m ; j++ ) {
		if ( ( t->fTab[i].fKey == KEY_FREE) || 
			( t->fTab[i].fKey == KEY_DELETED) ) {
			 t->fTab[i].fKey = key;
			 t->fTab[i].fData = d;
			t->fEntries += 1;
			return TRUE;
		}
		i = ( i + step ) % m;
	}

	qp_abort( ST(HashAdd) ": Table Full ??\n" );
}


bool
HashRemove(
	HashTable	t,
	char *		key
) {
	int	i, step, j, m;

	m =  1 << t->fBits;

	HashHash( t, key, &i, &step );

	for( j=0; j < m ; j++ ) {
		if ( t->fTab[i].fKey == KEY_FREE ) {
			return FALSE;
		}

		if ( t->fTab[i].fKey != KEY_DELETED ) {
			if ( strcasecmp( t->fTab[i].fKey, key ) == 0 ) {
				t->fTab[i].fKey = KEY_DELETED;
				t->fEntries -= 1;
				return TRUE;
			}
		}
		i = ( i + step ) % m;
	}
	qp_abort( ST(HashRemove) ": Table Full ??\n" );
}


bool
HashFind( HashTable t, char * key, HashData * d )
{
	int	i, step, j, m;

	m =  1 << t->fBits;

	HashHash( t, key, &i, &step );

	for( j=0; j < m ; j++ ) {
		if ( t->fTab[i].fKey == KEY_FREE ) {
			return FALSE;
		}

		if ( t->fTab[i].fKey != KEY_DELETED ) {
			if ( strcasecmp( t->fTab[i].fKey, key ) == 0 ) {
				*d = t->fTab[i].fData;
				return TRUE;
			}
		}
		i = ( i + step ) % m;
	}
	qp_abort( ST(HashFind) ": Table Full ??\n" );
}


void
HashDel( HashTable t )
{
	free( (void *) t->fTab );
	free( (void *) t );
}


#endif	/* defined(HASH_IMPLEMENT) */


#undef HC
#undef _HC
#undef HashSlot
#undef HashTable
#undef HashTableStruct

#undef HashNew
#undef HashGrow
#undef HashHash
#undef HashAdd
#undef HashRemove
#undef HashFind
#undef HashDel

#undef HASH_PREF
#undef HashData
#ifdef HASH_IMPLEMENT
#undef HASH_IMPLEMENT
#endif
