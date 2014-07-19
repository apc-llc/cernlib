/*
 *  smap.c  --
 *	Implement String Table, a String Vector with Hashed lookup
 *	which creates a fixed mapping between a string and an integer
 *
 *  Original: 10-Jan-1996 14:14
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: smap.c,v 1.11 2001/09/18 13:55:19 couet Exp $
 *
 *  $Log: smap.c,v $
 *  Revision 1.11  2001/09/18 13:55:19  couet
 *  - smap_sort is not used anymore. It has been removed.
 *
 *  Revision 1.10  1996/05/14 12:23:35  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.9  1996/04/30 10:06:46  maartenb
 *  - Fix double free of internal buffer for smap_sort()
 *
 *  Revision 1.8  1996/04/23 18:39:10  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<string.h>

#include	"qp_report.h"
#include	"smap.h"
#include	"str.h"


SMap
smap_new( int max )
{
	SMap	sm;

	sm = (SMap) calloc( sizeof(SMapStruct), 1 );
	qp_assert( sm );

	sm->fSize = max;

	sm->fV = svec_new( max );
	sm->fT = HashInt_new( max );

	return sm;
}


extern SMap
smap_copy( SMap old )
{
	SMap	new;
	int	i, n;

	new = smap_new( old->fSize );

	n = old->fEntries;
	for ( i=0 ; i < n ; i++ ) {
		smap_add( new, smap_get( old, i ) );
	}

	return new;
}


int
smap_add( SMap sm, String key )
{
	int	i;
	String	new_key;

	if ( HashInt_find( sm->fT, key, &i ) ) {
		return 0;
	}

	if ( sm->fEntries == sm->fSize ) {
		return 1;
	}

	sm->fEntries += 1;

	new_key = str_new( key );
	svec_add( sm->fV, new_key );
	HashInt_add( sm->fT, new_key, sm->fEntries );

	return 0;
}


int
smap_entries( SMap sm )
{
	return sm->fEntries;
}


String
smap_get( SMap sm, const int i )
{
	return svec_get( sm->fV, i );
}


bool
smap_map( SMap sm, String key, int * ip )
{
	return HashInt_find( sm->fT, key, ip );
}


void
smap_del( SMap sm )
{
	HashInt_del( sm->fT );
	svec_del( sm->fV );
	free( (void *) sm );
}
