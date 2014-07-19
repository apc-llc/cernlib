/*
 *  qp_convert.c  --
 *	Types and routines related to type promotion and conversion
 *
 *  Original: 25-Nov-1994 10:49
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_convert.c,v 1.7 2000/09/27 14:20:52 couet Exp $
 *
 *  $Log: qp_convert.c,v $
 *  Revision 1.7  2000/09/27 14:20:52  couet
 *  - The way the assert input parameter is passed is modified to bypass
 *    some bug in the HP cc compiler.
 *
 *  Revision 1.6  1996/04/23 18:38:12  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<assert.h>

#include	"qp_convert.h"
#include	"qp_dimension.h"
#include	"qp_semantic.h"


static DataType
op_result[D_DOUBLE-D_BOOL+1][D_DOUBLE-D_BOOL+1] = {
/*    (b)	B	U	LU	I	LI	F	LF	*/

/* (a) */
/* B */		D_BOOL,	D_UINT,	D_ULONG,D_INT,	D_LONG,	D_FLOAT,D_DOUBLE,

/* U */		D_UINT,	D_UINT,	D_ULONG,D_INT,	D_LONG,	D_FLOAT,D_DOUBLE,

/* LU */	D_ULONG,D_ULONG,D_ULONG,D_LONG,	D_LONG,	D_DOUBLE,D_DOUBLE,

/* I */		D_INT,	D_INT,	D_LONG,	D_INT,	D_LONG,	D_FLOAT,D_DOUBLE,

/* LI */	D_LONG,	D_LONG,	D_LONG,	D_LONG,	D_LONG,	D_DOUBLE,D_DOUBLE,

/* F */		D_FLOAT,D_FLOAT,D_DOUBLE,D_FLOAT,D_DOUBLE,D_FLOAT,D_DOUBLE,

/* LF */	D_DOUBLE,D_DOUBLE,D_DOUBLE,D_DOUBLE,D_DOUBLE,D_DOUBLE,D_DOUBLE,

};


static ConvType
promote_o1[D_DOUBLE-D_BOOL+1][D_DOUBLE-D_BOOL+1] = {
/*	    (b)	B	U	LU	I	LI	F	LF */

/*(a)*/
/* B */		NOCONV,	B_2_U,	B_2_LU,	B_2_I,	B_2_LI,	B_2_F,	B_2_LF,

/* U */		NOCONV,	NOCONV,	U_2_LU,	U_2_I,	U_2_LI,	U_2_F,	U_2_LF,

/* LU */	NOCONV,	NOCONV,	NOCONV,	LU_2_LI,LU_2_LI,LU_2_LF,LU_2_LF,

/* I */		NOCONV,	NOCONV,	I_2_LI,	NOCONV,	I_2_LI,	I_2_F,	I_2_LF,

/* LI */	NOCONV,	NOCONV,	NOCONV,	NOCONV,	NOCONV,	LI_2_LF,LI_2_LF,

/* F */		NOCONV,	NOCONV,	F_2_LF,	NOCONV,	F_2_LF,	NOCONV,	F_2_LF,

/* LF */	NOCONV,	NOCONV,	NOCONV,	NOCONV,	NOCONV,	NOCONV,	NOCONV,

};


static ConvType
map_in_to_out[D_DOUBLE-D_BOOL+1][D_DOUBLE-D_BOOL+1] = {
	NOCONV,	B_2_U,	B_2_LU,	B_2_I,	B_2_LI,	B_2_F,	B_2_LF,
	U_2_B,	NOCONV,	U_2_LU,	U_2_I,	U_2_LI,	U_2_F,	U_2_LF,
	LU_2_B,	LU_2_U,	NOCONV,	LU_2_I,	LU_2_LI,LU_2_F,	LU_2_LF,
	I_2_B,	I_2_U,	I_2_LU,	NOCONV,	I_2_LI,	I_2_F,	I_2_LF,
	LI_2_B,	LI_2_U,	LI_2_LU,LI_2_I,	NOCONV,LI_2_F,	LI_2_LF,
	F_2_B,	F_2_U,	F_2_LU,	F_2_I,	F_2_LI,	NOCONV,	F_2_LF,
	LF_2_B,	LF_2_U,	LF_2_LU,LF_2_I,	LF_2_LI,LF_2_F,	NOCONV,
};


static char *
_conv_to_str[C_LAST] = {
	"NOCONV",
	"B_2_U",
	"B_2_LU",
	"B_2_I",
	"B_2_LI",
	"B_2_F",
	"B_2_LF",
	"U_2_B",
	"U_2_LU",
	"U_2_I",
	"U_2_LI",
	"U_2_F",
	"U_2_LF",
	"LU_2_B",
	"LU_2_U",
	"LU_2_I",
	"LU_2_LI",
	"LU_2_F",
	"LU_2_LF",
	"I_2_B",
	"I_2_U",
	"I_2_LU",
	"I_2_LI",
	"I_2_F",
	"I_2_LF",
	"LI_2_B",
	"LI_2_U",
	"LI_2_LU",
	"LI_2_I",
	"LI_2_F",
	"LI_2_LF",
	"F_2_B",
	"F_2_U",
	"F_2_LU",
	"F_2_I",
	"F_2_LI",
	"F_2_LF",
	"LF_2_B",
	"LF_2_U",
	"LF_2_LU",
	"LF_2_I",
	"LF_2_LI",
	"LF_2_F"
};


char *
convtype_to_str( const ConvType c )
{
	return _conv_to_str[ c ];
}


void
resolve_type_3(
	pTree *		tree1,
	pTree *		tree2,
	pTree *		tree3,
	DataType *	result,
	int * r
) {
	pTree		t1 = *tree1, t2 = *tree2, t3 = *tree3;
	DataType	d1 = t1->dim->dtyp;
	DataType	d2 = t2->dim->dtyp;
	DataType	d3 = t3->dim->dtyp;
	DataType	dtyp;
	ConvType	c;
	int             c1;

	c1 = ( (d1==D_BOOL) || IS_NUMERIC(d1) ) &&
	     ( (d2==D_BOOL) || IS_NUMERIC(d2) ) &&
	     ( (d3==D_BOOL) || IS_NUMERIC(d3) );
        
	assert(c1);

	/* determine result type */
	dtyp = op_result[d1][d2];
	dtyp = op_result[dtyp][d3];



	c = map_in_to_out[ d1 ][ dtyp ];
	if ( c != NOCONV ) {
		*tree1 = sem_check(
			new_conv_node( t1, c ),
			r );
	}

	c = map_in_to_out[ d2 ][ dtyp ];
	if ( c != NOCONV ) {
		*tree2 = sem_check(
			new_conv_node( t2, c ),
			r );
	}

	c = map_in_to_out[ d3 ][ dtyp ];
	if ( c != NOCONV ) {
		*tree3 = sem_check(
			new_conv_node( t3, c ),
			r );
	}

	*result = dtyp;
}


void
resolve_type_promotion( pTree *tree1, pTree *tree2, DataType *result, int * r )
{
	pTree		t1 = *tree1, t2 = *tree2;
	DataType	d1 = t1->dim->dtyp, d2 = t2->dim->dtyp;
	ConvType	c;
	int             c1;

	c1 = ( (d1==D_BOOL) || IS_NUMERIC(d1) ) &&
	     ( (d2==D_BOOL) || IS_NUMERIC(d2) );

	assert(c1);

	c = promote_o1[ d1 ][ d2 ];
	if ( c != NOCONV ) {
		*tree1 = sem_check(
			new_conv_node( t1, c ),
			r );
	}

	c = promote_o1[ d2 ][ d1 ];
	if ( c != NOCONV ) {
		*tree2 = sem_check(
			new_conv_node( t2, c ),
			r );
	}

	*result = op_result[ d2 ][ d1 ];
}


ConvType
convert_type( DataType old, DataType new ) {

	assert( old != new );

	return map_in_to_out[old][new];
}
