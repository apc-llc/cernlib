/*
 *  qp_convert.h  --
 *	Types and routines related to type promotion and conversion
 *
 *  Original: 25-Nov-1994 13:59
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_convert.h,v 1.3 1996/04/23 18:38:12 maartenb Exp $
 *
 *  $Log: qp_convert.h,v $
 *  Revision 1.3  1996/04/23 18:38:12  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_CONVERT
#define CERN_CONVERT


typedef enum _conv_type_ {	/* forward declaration */
	NOCONV,
	B_2_U, B_2_LU, B_2_I, B_2_LI, B_2_F, B_2_LF,
	U_2_B, U_2_LU, U_2_I, U_2_LI, U_2_F, U_2_LF,
	LU_2_B, LU_2_U, LU_2_I, LU_2_LI, LU_2_F, LU_2_LF,
	I_2_B, I_2_U, I_2_LU, I_2_LI, I_2_F, I_2_LF,
	LI_2_B, LI_2_U, LI_2_LU, LI_2_I, LI_2_F, LI_2_LF,
	F_2_B, F_2_U, F_2_LU, F_2_I, F_2_LI, F_2_LF,
	LF_2_B, LF_2_U, LF_2_LU, LF_2_I, LF_2_LI, LF_2_F,
	C_LAST
} ConvType;


#include	"qp_tree.h"


#define	IS_NUMERIC( d )	\
		( (d) >= D_UINT && (d) <= D_DOUBLE )


char *
convtype_to_str( const ConvType c );

void
resolve_type_promotion( pTree *tree1, pTree *tree2,
			DataType *result, int * r );

ConvType
convert_type( DataType old, DataType new );

#endif	/*	CERN_CONVERT	*/
