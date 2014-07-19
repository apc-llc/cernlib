/*
 *  qp_signature.h  --
 *	Signatures hold the static type info associated 
 *	with symbol table entries.
 *
 *  Original: 10-Oct-1994 13:56
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_signature.h,v 1.8 1996/04/23 18:39:01 maartenb Exp $
 *
 *  $Log: qp_signature.h,v $
 *  Revision 1.8  1996/04/23 18:39:01  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_SIGNATURE
#define CERN_SIGNATURE


typedef struct _signature_ Signature;


#include	"cern_types.h"
#include	"hbook_defs.h"
#include	"fcode.h"
#include	"qp_value.h"
#include	"qp_dimension.h"
#include	"qp_mask.h"

typedef enum _sig_type_	{
	S_FUN = 1,	/* builtin function */
	S_CVAR,		/* cwn ntup var */
	S_RVAR,		/* rwn ntup var */
	S_VEC,		/* kuip vector */
	S_COMIS,	/* comis routine */
	S_MASK		/* mask */
} SigType;



typedef struct _sig_const_ {
	Value	*val;
} SigConst;


typedef struct _sig_fun_ {
	FCode		fc;
	int		argc;
	Dimension	*argv[MAX_DIMENSION];
} SigFun;


typedef struct _sig_cwn_ {
	int	index;
	char	block[MAX_BLOCK_LEN];
	int	size;		/* in Int32's */
	int	elem;
} SigCWN;


typedef struct _sig_rwn_ {
	int	index;
} SigRWN;


typedef struct _sig_comis_ {
	int		iaddr;
	int		argc;
	Dimension	*argv[MAX_DIMENSION];
	char **		namelist;		/* list of common block member used in */
						/* routines called by this routine */
	/* extra info */
} SigComis;


typedef struct _sig_mask_ {
	Mask	*mp;
} SigMask;


typedef struct _sig_vec_ {
	int	dummy;
} SigVec;


struct _signature_ {
	Signature	*next;
	SigType		typ;
	Dimension	*dim;
	union {
		SigConst	con;
		SigFun		fun;
		SigCWN		cvar;
		SigRWN		rvar;
		SigMask		mask;
		SigComis	comis;
		SigVec		vec;
	} u;
};


Signature *
sig_new_scalar_double( double d );

Signature *
sig_new_scalar_int( int	ival );

Signature *
sig_new_scalar_uint( int uval );

Signature *
sig_new_scalar_bool( bool bval );

Signature *
sig_new_scalar_str( char *sval );

Signature *
sig_new_cvar( Dimension * d, char * block, int isize, int ielem );

Signature *
sig_new_rvar( int index );

Signature *
sig_new_mask( Mask * mp );

Signature *
sig_new_vec( Dimension * d );

Signature *
sig_new_fun( FCode fc, Dimension * d, int argc,
	Dimension *argv[MAX_DIMENSION] );

Signature *
sig_new_comis(
	int		iaddr,	/* the comis ref to the routine */
	Dimension	*d,	/* the return type of the function */
	int		argc,	/* number of args, -1 for unknown */
	Dimension	*argv[MAX_DIMENSION] /* the arguments */
);

void
sig_del( Signature * st );

#endif	/*	CERN_SIGNATURE	*/
