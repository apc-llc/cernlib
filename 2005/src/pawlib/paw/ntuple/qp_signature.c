/*
 *  qp_signature.c  --
 *	Create signatures.
 *
 *  Original: 19-Oct-1994 17:07
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_signature.c,v 1.10 1996/05/14 12:23:33 maartenb Exp $
 *
 *  $Log: qp_signature.c,v $
 *  Revision 1.10  1996/05/14 12:23:33  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.9  1996/04/23 18:39:01  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>
#include	<string.h>

#include	"str.h"
#include	"qp_report.h"
#include	"qp_signature.h"


static Signature *
sig_alloc()
{
	Signature	*s;

	s = (Signature *) calloc( sizeof(Signature), 1 );

	if ( s == 0 ) {
		qp_abort( "Cannot allocate memory for 'Signature' struct.\n" );
	}

	return s;
}


Signature *
sig_new_fun( FCode fc, Dimension *d, int argc, Dimension *argv[MAX_DIMENSION] )
{
	Signature	*s;
	int	i;

	s = sig_alloc();
	
	s->typ = S_FUN;

	s->dim = dim_copy( d );

	s->u.fun.fc = fc;
	s->u.fun.argc = argc;
	for( i=0; i < argc ; i++ )
		s->u.fun.argv[i] = dim_copy( argv[i] );

	return s;
}


Signature *
sig_new_comis(
	int		iaddr,	/* the comis ref to the routine */
	Dimension	*d,	/* the return type of the function */
	int		argc,	/* number of args, -1 for unknown */
	Dimension	*argv[MAX_DIMENSION] /* the arguments */
) {
	Signature	*s;
	int		i;

	s = sig_alloc();
	
	s->typ = S_COMIS;

	s->dim = dim_copy( d );

	s->u.comis.iaddr = iaddr;
	s->u.comis.argc = argc;
	for ( i=0 ; i < argc ; i++ ) {
		s->u.comis.argv[i] = dim_copy( argv[i] );
	}

	return s;
}


Signature *
sig_new_rvar( int index ) {
	Signature	*s;

	s = sig_alloc();
	
	s->typ = S_RVAR;

	s->dim = dim_new( D_FLOAT );

	s->u.rvar.index = index;

	return s;
}


Signature *
sig_new_mask( Mask * mp) {
	Signature	*s;

	s = sig_alloc();
	
	s->typ = S_MASK;

	s->dim = dim_new( D_UINT );	/* becomes bool when indexed */

	s->u.mask.mp = mp;

	return s;
}


Signature *
sig_new_cvar( Dimension * dim, char * block, int isize, int ielem ) {
	Signature	*s;

	s = sig_alloc();
	
	s->typ = S_CVAR;

	s->dim = dim_copy( dim );

	strncpy( s->u.cvar.block, block, MAX_BLOCK_LEN );
	s->u.cvar.size = isize / sizeof( Int32 );
	s->u.cvar.elem = ielem;

	return s;
}


Signature *
sig_new_vec( Dimension * dim ) {
	Signature	*s;

	s = sig_alloc();
	
	s->typ = S_VEC;

	s->dim = dim_copy( dim );

	return s;
}


void
sig_del( Signature * s )
{
	switch ( s->typ ) {	/* switch SigType */
	case S_FUN:		/* builtin function */
		break;
	case S_CVAR:		/* cwn ntup var */
		break;
	case S_RVAR:		/* rwn ntup var */
		break;
	case S_VEC:		/* kuip vector */
		break;
	case S_COMIS:	/* comis routine */
		{
			int	i;
		
			for ( i=0 ; i < s->u.comis.argc ; i++ ) {
				dim_del( s->u.comis.argv[i] );
			}

			if ( s->u.comis.namelist != 0 ) {
				for ( i=0 ; s->u.comis.namelist[i] != 0 ; i++ ) {
					str_del( s->u.comis.namelist[i] );
				}
				free( (void *) s->u.comis.namelist );
			}
		}
		break;

	case S_MASK:		/* mask */
		break;
	default:
		qp_abort( "sig_del: Unknown SigType %d\n", s->typ );
		break;
	}

	if ( s->dim != 0 ) {
		dim_del( s->dim );
	}

	free( (void *) s );
}
