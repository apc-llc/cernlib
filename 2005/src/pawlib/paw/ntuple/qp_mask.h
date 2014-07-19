/*
 *  qp_mask.h  --
 *	Description of the massk involved in a query
 *
 *  Original: 24-Nov-1994 11:09
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_mask.h,v 1.5 1996/04/23 18:38:46 maartenb Exp $
 *
 *  $Log: qp_mask.h,v $
 *  Revision 1.5  1996/04/23 18:38:46  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_MASK
#define CERN_MASK

#include	"cern_types.h"


#define	MASK_BLOCK_SIZE		1024	/* gives a 4k block */

typedef struct _bit_desc_ {
	bool		valid;
	char		*desc;
	int		selected;	/* number of events passing the cut */
} BitDesc;


typedef struct _mask_ {
	char		*mname;
	char		*fname;
	FILE		*mfp;
	Int32		nevt;		/* number of events in the mask */
	bool		update;
	bool		dirty;		
	BitDesc		bits[32];
	Int32		buf_index;
	Int32		buf[MASK_BLOCK_SIZE];
} Mask;

Mask *
mask_find( const char * const mname );

bool
mask_bit_defined( const Mask * const mp, const int ibit );

bool
mask_writable( const Mask * const mp );

UInt32
mask_get_word( Mask * mp, long ievt, int * errp );

void
mask_set_desc( Mask *mp, int ibit, char * desc );

void
mask_set_bit( Mask * mp, long ievt, int ibit, bool value, int * errp );

int
mask_flush_header( Mask * mp );

int
mask_flush_buf( Mask * mp );

#endif	/*	CERN_MASK	*/
