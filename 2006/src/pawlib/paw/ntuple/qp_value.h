/*
 *  qp_value.h  --
 *	Store for possibly multi-dimensional value.
 *
 *  Original:  1-Nov-1994 15:08
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_value.h,v 1.4 1996/04/23 18:39:08 maartenb Exp $
 *
 *  $Log: qp_value.h,v $
 *  Revision 1.4  1996/04/23 18:39:08  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_VALUE
#define CERN_VALUE

#include	<stdio.h>


#include	"cern_types.h"


typedef struct _value_ Value;


#include	"fcode.h"
#include	"qp_dimension.h"
#include	"qp_signature.h"



typedef union {
		/* scalar values */
		bool		bval;
		UInt32		uval;
		UInt64		luval;
		Int32		ival;
		Int64		lival;
		Float32		fval;
		Float64		lfval;
		String		sval;
		/* multi dim array values */
		bool		*vbval;
		UInt32		*vuval;
		Int32		*vival;
		Float32		*vfval;
		Float64		*vdval;
		String		*vsval;
		/* Acces via Int32 array */
		Int32		buf[2];
} Uval;


struct _value_ {
	Dimension	*d;
	Uval		u;
};

void
val_print( FILE * fp, Value * v );

void
val_dump( FILE * fp, int lvl, Value * v );

Value *
val_eval( FCode fc, int valc, Value *valv[MAX_DIMENSION], int * err );

Value *
val_comis( Signature * s, int valc, Value *valv[MAX_DIMENSION],  int * err );

Value *
val_new_bool( bool bval );

Value *
val_new_uint( UInt32 uval );

Value *
val_new_ulong( UInt64 uval );

Value *
val_new_int( Int32 ival );

Value *
val_new_float( Float64 lfval );

Value *
val_new_double( Float64 lfval );

Value *
val_new_vfloat( Dimension *d, Float32 *vfval );

Value *
val_new_vint( Dimension *d, Int32 *vival );

Value *
val_new_str( String sval );

Value *
val_copy( Value * v );

void
val_del( Value * v );

#endif	/*	CERN_VALUE	*/
