/*
 *  qp_vvec.h  --
 *	Implement a vector of var records.
 *	(This should be templatized, with svec etc)
 *
 *  Original: 10-May-1996 16:03
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_vvec.h,v 1.1 1996/05/12 15:13:56 maartenb Exp $
 *
 *  $Log: qp_vvec.h,v $
 *  Revision 1.1  1996/05/12 15:13:56  maartenb
 *  - Replaced the handling of variables. The handling of variables is now
 *  fully dynamic and there is no other limit than the commonblocks to the
 *  number of var's in a query.
 *
 *
 *
 */

#ifndef CERN_QP_VVEC
#define CERN_QP_VVEC



#include	"cern_types.h"
#include	"hbook_defs.h"
#include	"qp_dimension.h"


typedef enum {
	Var_RWN, Var_CWN, Var_Vec
} VarClass;


typedef struct _var_struct_ * Variable;
typedef struct _var_struct_ {
	VarClass	var_class;
	char		block[MAX_BLOCK_LEN+1];
	char		name[MAX_NAME_LEN+1];
	DataType	dtyp;
	int		size;		/* size in Int32's */
	bool		varDim;		/* is variable length array ? */
	int		var_base;	/* number of elements in all the non */
					/* variable dimensions ( = total if */
					/* varDim = FALSE) */
	int		var_index;	/* index of the index variable */
} VarStruct;


typedef struct _vvec_struct_ {
	int		fSize;
	int		fEntries;
	Variable *	fV;
} VVecStruct;

typedef VVecStruct	*VVec;


extern Variable
var_alloc();


extern Variable
var_new( Variable v );


extern void
var_del( Variable v );


extern VVec
vvec_new( int n );

extern VVec
vvec_copy( VVec vv );

extern void
vvec_add( VVec vv, Variable v );

extern Variable
vvec_get( VVec vv, const int i );

extern void
vvec_del( VVec vv );

extern int
vvec_entries( VVec vv );

#endif	/*	CERN_QP_VVEC	*/
