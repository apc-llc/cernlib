/*
 *  qp_generate.c  --
 *	Generation of QP assembly from a fully decorated
 *	parse tree.
 *
 *  Original: 24-Nov-1994 10:44
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_generate.c,v 1.24 1999/07/05 15:43:34 couet Exp $
 *
 *  $Log: qp_generate.c,v $
 *  Revision 1.24  1999/07/05 15:43:34  couet
 *  - hbook_interface.h in now replaced by hbook.h in CVSCOSRC
 *
 *  Revision 1.23  1999/06/28 15:09:01  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.22  1996/10/17 15:38:12  couet
 *  - missing prototype for qp_dyn_list
 *
 *  Revision 1.21  1996/06/07 07:29:37  couet
 *  - In the counting of ntuple variables used in a comis function, the
 *    test to check if a variable was already counted was incorrect. I was done
 *    with a string comparison between two strings but in a case senvitive
 *    way. Now it is case unsensitive (toupper is called).
 *
 *  Revision 1.20  1996/05/12 15:13:53  maartenb
 *  - Replaced the handling of variables. The handling of variables is now
 *  fully dynamic and there is no other limit than the commonblocks to the
 *  number of var's in a query.
 *
 *  Revision 1.19  1996/04/23 18:38:41  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>

#include	"str.h"	/* placed because of IRIX _INT in stdarg.h */
#include	<cfortran/cfortran.h>

#include	"errors.h"
#include	"fcode.h"
#include	<cfortran/hbook.h>
#include	"qpflags.h"
#include	"qp_query.h"
#include	"qp_report.h"
#include	"qp_dimension.h"
#include	"qp_generate.h"
#include	"qp_segment.h"
#include	"qp_signature.h"
#include	"qp_symtab.h"
#include	"qp_tree.h"

/* extern functions */
extern void qp_dyn_list( bool, int, pTree *, int *);

static SegBuild		*the_sbp;

static void
gen_tree( QueryExe * qe, pTree t, int * errp );


/** tree list blob **/

typedef struct _tree_list_ {
	int	n;
	int	size;
	pTree	*list;
} TreeList;


static TreeList *
tl_new( int * errp )
{
	TreeList	*tl;

	tl = (TreeList *) calloc( sizeof(TreeList), 1 );
	if ( tl == 0 ) {
		*errp = R_ALLOC_ERROR;
		sf_report( "tl_new: Allocation error\n" );
		return 0;
	}

	tl->list = (pTree *) calloc( sizeof(pTree), 5 );
	if ( tl->list == 0 ) {
		*errp = R_ALLOC_ERROR;
		sf_report( "tl_new: Allocation error\n" );
		return 0;
	}

	tl->size = 5;

	return tl;
}


static void
tl_free( TreeList *tl )
{
	free( tl->list );
	free( tl );
}


static void
tl_add( TreeList *tl, pTree t, int * errp )
{
	if ( tl->n == tl->size ) {
		tl->size += 5;
		tl->list = (pTree *) realloc( tl->list,
				tl->size * sizeof(pTree) );
		if ( tl->list == 0 ) {
			*errp = R_ALLOC_ERROR;
			sf_report( "tl_add: Allocation error\n" );
			return;
		}
	}

	tl->list[ tl->n++ ] = t;
}


static void
split_tree( pTree t, TreeList *tl_sca, TreeList *tl_vec, int * errp )
{
	pTree	o;
	OpType	op;

	op = t->u.op.op;

	o = t->u.op.o1;
	if ( o->ntyp == N_OP && (o->u.op.op == op) ) {
		split_tree( o, tl_sca, tl_vec, errp );
	} else {
		if ( o->dim->ndim == 0 ) {
			tl_add( tl_sca, o, errp );
		} else {
			tl_add( tl_vec, o, errp );
		}
	}

	if ( *errp != R_NOERR ) 
		return;

	o = t->u.op.o2;
	if ( o->ntyp == N_OP && (o->u.op.op == op) ) {
		split_tree( o, tl_sca, tl_vec, errp );
	} else {
		if ( o->dim->ndim == 0 ) {
			tl_add( tl_sca, o, errp );
		} else {
			tl_add( tl_vec, o, errp );
		}
	}
}


static void
gen_op_andor( QueryExe * qe, pTree t, int * errp )
{
	TreeList	*tl_sca;
	TreeList	*tl_vec;
	FCode		fc, jump_code;
	OpType		op;
	int		i, lid, check_shape;
	UInt32		flags;

	tl_sca = tl_new( errp );
	if ( *errp != R_NOERR ) {
		return;
	}

	tl_vec = tl_new( errp );
	if ( *errp != R_NOERR ) {
		return;
	}

	op = t->u.op.op;
	/* always set the vector bit (!) */
	fc = D_BOOL << 6 | optype_to_fc( op ) | FC_VECTOR_BIT;
	jump_code = op == O_AND ? FC_POP_JUMP : FC_JUMP_POP;

	split_tree( t, tl_sca, tl_vec, errp );

	lid = qp_seg_new_label( the_sbp, errp );

	for ( i=0 ; (*errp == R_NOERR) && (i < tl_sca->n) ; i++ ) {
		gen_tree( qe, tl_sca->list[i], errp );
		if ( *errp != R_NOERR ) continue;
		if ( (i == (tl_sca->n - 1)) && ( tl_vec->n == 0 ) ) {
			/* if there are no non-scalar terms for this	*/
			/* sequence then suppress the jump after	*/
			/* this term					*/
			continue;
		}
		qp_seg_emit_opc( the_sbp, jump_code, errp );
		qp_seg_emit_ref( the_sbp, lid, errp );
	}

	
	qp_dyn_list( t->need_shape, tl_vec->n, tl_vec->list, &check_shape );

	flags = 0;
	if ( check_shape > 0 ) {
		flags |= FC_DYNAMIC_TEST_BIT;
	}
	if ( t->need_shape ) {
		flags |= FC_DYNAMIC_INFO_BIT;
	}

	if ( *errp == R_NOERR ) {
		if ( tl_vec->n != 0 ) {
			for ( i=0 ; i < tl_vec->n ; i++ ) {
				gen_tree( qe, tl_vec->list[i], errp );
				if ( *errp != R_NOERR ) continue;
				if ( i > 0 ) {
					if ( i == tl_vec->n-1 ) {
						qp_seg_emit_opc( the_sbp,
							flags | fc, errp );
						if ( check_shape > 0 )
							qp_seg_emit_word(
								the_sbp,
								check_shape,
								errp );
					} else {
						qp_seg_emit_opc( the_sbp,
							fc, errp );
					}
				}
			}
		}

		qp_seg_emit_label( the_sbp, lid, errp );
	}


	tl_free( tl_sca );
	tl_free( tl_vec );
}


static void
gen_op( QueryExe * qe, pTree t, int * errp )
{
	FCode	fc;

	
	fc = t->u.op.o1->dim->dtyp << 6 | optype_to_fc( t->u.op.op );

	if ( t->dim->ndim != 0 ) {
		fc = fc | FC_VECTOR_BIT;
	}

	if ( t->check_shape > 0 ) {
		fc = fc | FC_DYNAMIC_TEST_BIT;
	}

	if ( t->need_shape ) {
		fc = fc | FC_DYNAMIC_INFO_BIT;
	}

	switch ( t->u.op.op ) {	/* switch DataType */

	case O_OR:	/* boolean operators have "jump" implementation */
	case O_AND:	/* this involves grouping of the top level   */
			/* and/or's and a label tabel for the segment ..    */

			gen_op_andor( qe, t, errp );
			return;
			break;

	case O_LT:
	case O_LE:
	case O_GT:
	case O_GE:

	case O_EQ:
	case O_NE:
	case O_CT:

	case O_PLUS:
	case O_MINUS:
	case O_TIMES:
	case O_DIV:
	case O_POW:
			gen_tree( qe, t->u.op.o2, errp );
			if ( *errp != R_NOERR ) break;
			gen_tree( qe, t->u.op.o1, errp );
			break;

	case O_LTLT:
	case O_LTLE:
	case O_LELT:
	case O_LELE:
	case O_GTGT:
	case O_GTGE:
	case O_GEGT:
	case O_GEGE:
			gen_tree( qe, t->u.op.o3, errp );
			if ( *errp != R_NOERR ) break;
			gen_tree( qe, t->u.op.o2, errp );
			if ( *errp != R_NOERR ) break;
			gen_tree( qe, t->u.op.o1, errp );
			break;

	case O_NOT:
	case O_UMINUS:
			gen_tree( qe, t->u.op.o1, errp );
			break;

	default:
		sf_report( "gen_op: Unkown OpType (%d)\n", t->u.op.op );
		*errp = R_INTERNAL_ERROR;
		break;
	}

	if ( *errp == R_NOERR ) {
		qp_seg_emit_opc( the_sbp, fc, errp );
	}

	if ( t->check_shape > 0 ) {
		qp_seg_emit_word( the_sbp, t->check_shape, errp );
	}

}


static void
gen_const( QueryExe * qe, pTree t, int * errp )
{
	if ( t->dim->ndim != 0 ) {
		sf_report( "gen_const: non scalar literals not implemented\n");
		*errp = R_NOT_IMPLEMENTED;
		return;
	}

	if ( (t->check_shape > 0) || t->need_shape ) {
		sf_report( "gen_const: Dynamic shapes not "
			"implemented\n" );
		*errp = R_NOT_IMPLEMENTED;
		return;
	}

	qp_assert( t->val != 0 );

	switch ( t->val->d->dtyp ) {	/* switch DataType */
	case D_BOOL:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_BOOL, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			break;
	case D_UINT:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_UINT, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			break;
	case D_ULONG:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_ULONG, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[1], errp );
			break;
	case D_INT:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_INT, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			break;
	case D_LONG:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_LONG, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[1], errp );
			break;
	case D_FLOAT:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_FLOAT, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			break;
	case D_DOUBLE:
			qp_seg_emit_opc( the_sbp, FC_LIT_SCA_DOUBLE, errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[0], errp );
			qp_seg_emit_word( the_sbp, t->val->u.buf[1], errp );
			break;
	case D_STR:
			{
				Int32	buf[8];
				int	i;

				memset( buf, ' ', QP_STR_MAX );
				strncpy( (char *) buf, t->val->u.sval,
					strlen(t->val->u.sval) );
				qp_seg_emit_opc( the_sbp, FC_LIT_SCA_STR, errp );
				for( i=0 ; i < 8 ; i++ ) {
					qp_seg_emit_word( the_sbp, buf[i], errp );
				}
			}
			break;
	default:
		sf_report( "gen_const: Unkown DataType (%d)\n",
			t->val->d->dtyp );
		*errp = R_INTERNAL_ERROR;
		break;
	}

}


static int
alloc_var( QueryExe * qe, VarClass vc, char * name, int * errp )
{
	int		index;		/* index for this variable */
	int		var_index;	/* index for the var-lenght dimension */
	int		i, n;
	STEntry		*ep, *epidx;
	Signature	*sig;
	VVec		vv;
	Variable	v;
	bool		varDim;

	/* See if this cvar is already in the list */

	vv = qe->vv;
	n = vvec_entries( vv );

	index = -1;
	for ( i=0 ; i < n ; i++ ) {
	        char            *s1;
	        char            *s2;
		s1 = str_toupper( vvec_get( vv, i )->name );
		s2 = str_toupper( name );
		/*
		if ( strcmp( vvec_get( vv, i )->name, name ) == 0 ) {
		*/
		if ( strcmp( s1, s2 ) == 0 ) {
			index = i;
		        str_del( s1 );
	        	str_del( s2 );
			break;
		}
		str_del( s1 );
		str_del( s2 );
	}

	if ( index == -1 ) { /* allocate and fill a new slot */

		ep = st_lookup( sf_symtab, name ); qp_assert( ep != 0 );
		sig = ep->sig_list;

		/* can only occur for CWN */
		varDim = sig->dim->idx == STI_INVALID ? FALSE : TRUE;
		if ( varDim ) {
			epidx = st_entry( sf_symtab, sig->dim->idx );
			var_index = alloc_var( qe, Var_CWN,
					epidx->name, errp );
		}

		v = var_alloc();
		index = vvec_entries( vv );
		vvec_add( vv, v );

		strncpy( v->name, ep->name, MAX_NAME_LEN );
		v->name[MAX_NAME_LEN] = '\0';
		v->dtyp = sig->dim->dtyp;
		v->varDim = varDim;

		v->var_class = vc;
		if ( vc == Var_CWN ) {
			strncpy( v->block, sig->u.cvar.block, MAX_BLOCK_LEN );
			v->block[MAX_BLOCK_LEN] = '\0';
			v->size = sig->u.cvar.size * sig->u.cvar.elem;
			if ( varDim ) {
				v->var_base = sig->u.cvar.elem /
					sig->dim->range[sig->dim->ndim-1];
				v->var_index = var_index;
			} else {
				v->var_base = sig->u.cvar.elem;
			}
		} else if ( vc == Var_RWN ) {
			*v->block = '\0';
			v->size = 1;
			v->var_base = 1;
		} else if ( vc == Var_Vec ) {
			int		size;

			for ( size = 1, i = 0 ; i < sig->dim->ndim ; i++ ) {
				size *= sig->dim->range[i];
			}

			*v->block = '\0';
			v->size = size;
			v->var_base = size;

		} else {
			qp_abort( "alloc_var: illegal VarClass %d\n", vc );
		}

		qe->total_size = (qe->total_size + ~datatype_align[v->dtyp])
				& datatype_align[v->dtyp];
		qe->total_size += v->size;
	}

	return index;
}


static void
gen_mask_read( QueryExe * qe, pTree t, int * errp )
{
	Mask		*mp;
	int		i, idx = -1;

	mp = mask_find( t->u.name.name ); qp_assert( mp != 0 );

	for ( i=0 ; i < qe->nmasks ; i++ ) {
		if ( qe->mask[i] == mp ) {
			idx = i;
			break;
		}
	}

	if ( idx == -1 ) {
		if ( qe->nmasks == MAX_MASKS ) {
			qp_abort( "gen_mask_read: Maximum number of masks "
				"reached (%d)\n", MAX_MASKS );
		}
		qe->mask[qe->nmasks] = mp;
		idx = qe->nmasks;
		qe->nmasks += 1;
	}

	if ( t->u.name.argc == 0 ) {
		qp_seg_emit_opc( the_sbp, FC_MASK_GET_WORD, errp );
	} else {
		qp_seg_emit_opc( the_sbp, FC_MASK_GET_BIT, errp );
	}

	qp_seg_emit_word( the_sbp, idx, errp );

	if ( t->u.name.argc == 1 ) {
		int	v;

		v = t->u.name.argv[0]->val->u.ival;
		qp_seg_emit_word( the_sbp, v, errp );
	}
}


static void
gen_mask_write( QueryExe * qe, pTree t, int * errp )
{
	Mask		*mp;
	int		i, v, idx = -1;

	mp = mask_find( t->u.mask.name );

	for ( i=0 ; i < qe->nmasks ; i++ ) {
		if ( qe->mask[i] == mp ) {
			idx = i;
			break;
		}
	}

	if ( idx == -1 ) {
		if ( qe->nmasks == MAX_MASKS ) {
			qp_abort( "gen_mask_write: Maximum number of masks "
				"reached (%d)\n", MAX_MASKS );
		}
		qe->mask[qe->nmasks] = mp;
		idx = qe->nmasks;
		qe->nmasks += 1;
	}

	gen_tree( qe, t->u.mask.o1, errp );

	v = t->u.mask.index->val->u.ival;
	qe->mask_write = idx;
	qe->mask_ibit = v;

	qp_seg_emit_opc( the_sbp, FC_MASK_SET_BIT, errp );
	qp_seg_emit_word( the_sbp, idx, errp );
	qp_seg_emit_word( the_sbp, v, errp );

}


/*
 * The ntuple variable access
 */

typedef struct {
	int	stride;
	int	start;
	pTree	start_tree;
	int	end;
	pTree	end_tree;
	int	max;
} DimRec;

static void
gen_cvar_vec( QueryExe * qe, pTree t, int * errp )
{
	DimRec		dim_list[MAX_DIMENSION];
	int		index, i, stride;
	bool		full_array;
	pTree		tp, to, from;
	Signature	*sig;
	DimRec		*a;
	bool		varlen, is_index;
	FCode		fc;
	UInt32		flags = 0;
	VarClass	vc;


	sig = t->u.name.sig;
	if ( sig->typ == S_CVAR ) {
		vc = Var_CWN;
	} else { 
		vc = Var_Vec;
	}

	index = alloc_var( qe, vc, t->u.name.name, errp );
	if ( *errp != R_NOERR ) {
		return;
	}

	if ( t->need_shape ) {
		flags |= FC_DYNAMIC_INFO_BIT;
	}

	qp_assert( t->check_shape == 0 );	/* should never check a leaf node */

	if ( sig->dim->ndim == 0 ) {

		switch ( t->dim->dtyp ) {	/* switch DataType */
		case D_BOOL:	fc = FC_CWN_SCA_BOOL; break;
		case D_UINT:	fc = FC_CWN_SCA_UINT; break;
		case D_ULONG:	fc = FC_CWN_SCA_ULONG; break;
		case D_INT:	fc = FC_CWN_SCA_INT; break;
		case D_LONG:	fc = FC_CWN_SCA_LONG; break;
		case D_FLOAT:	fc = FC_CWN_SCA_FLOAT; break;
		case D_DOUBLE:	fc = FC_CWN_SCA_DOUBLE; break;
		case D_STR:	fc = FC_CWN_SCA_STR; break;
		default:
			sf_report( "gen_cvar_vec: Unkown DataType (%d)\n",
				t->val->d->dtyp );
			*errp = R_INTERNAL_ERROR;
			break;
		}
		qp_seg_emit_opc( the_sbp, fc , errp );
		qp_seg_emit_word( the_sbp, index, errp );

		return;
	}

	flags |= FC_VECTOR_BIT;

	if ( t->dim->dtyp == D_STR && t->u.name.range != 0 ) {
		sf_report( "gen_cvar_vec: Subrange of string not implemented"
			" (ignored)\n" );
	}

	/* decide on sub-part .... */
	full_array = TRUE;
	stride = 1;
	for ( i=0 ; i < t->u.name.argc ; i++ ) {

		tp = t->u.name.argv[i];
		a = &dim_list[i];
		a->stride = stride;
		varlen = ( (i+1 == t->u.name.argc) && (sig->dim->idx != STI_INVALID) );

		if ( tp->ntyp != N_RANGE ) {
			if ( tp->val != 0 ) {
				a->start = tp->val->u.ival;
			} else {
				a->start = 0;
				a->start_tree = tp;
			}
			a->end = -1;	/* use the same value as start */
			/* happily ignore that dim of length 1 might exist */
			full_array = FALSE;
		} else {
			/* process the from value */
			from = tp->u.range.from;
			if ( from->val != 0 ) {
				a->start = from->val->u.ival;
				if ( a->start != 1 ) {
					full_array = FALSE;
				}
			} else {
				a->start = 0;
				a->start_tree = from;
				full_array = FALSE;
			}

			/* process the to value */
			to = tp->u.range.to;
			if ( to->val != 0 ) {
				a->end = to->val->u.ival;
				if ( a->end != sig->dim->range[i] ) {
					full_array = FALSE;
				}
			} else {
				is_index = varlen && (to->ntyp == N_NAME) &&
					(st_index(sf_symtab, to->u.name.name) == sig->dim->idx);

				if ( is_index ) {
					a->end = -2;	/* use the same value as max */
				} else {
					a->end = 0;
					a->end_tree = to;
					full_array = FALSE;
				}
			}
		}

		if ( varlen ) {
			/* allocate the dynamic length var also ... */
			int	idx;
			char	*name = st_entry( sf_symtab, sig->dim->idx )->name;
			idx = alloc_var( qe, Var_CWN, name, errp );
			a->max = -idx;
		} else {
			a->max = sig->dim->range[i];
		}

		stride *= sig->dim->range[i];
	}

	if ( full_array ) {

		switch ( t->dim->dtyp ) {	/* switch DataType */
		case D_BOOL:	fc = FC_CWN_MAT_BOOL; break;
		case D_UINT:	fc = FC_CWN_MAT_UINT; break;
		case D_ULONG:	fc = FC_CWN_MAT_ULONG; break;
		case D_INT:	fc = FC_CWN_MAT_INT; break;
		case D_LONG:	fc = FC_CWN_MAT_LONG; break;
		case D_FLOAT:	fc = FC_CWN_MAT_FLOAT; break;
		case D_DOUBLE:	fc = FC_CWN_MAT_DOUBLE; break;
		case D_STR:	fc = FC_CWN_MAT_STR; break;
		default:
			sf_report( "gen_cvar_vec: Unkown DataType (%d)\n",
				t->val->d->dtyp );
			*errp = R_INTERNAL_ERROR;
			break;
		}
		qp_seg_emit_opc( the_sbp, fc | flags, errp );
		qp_seg_emit_word( the_sbp, index, errp );

		if ( t->need_shape ) {
			int	i;
			qp_seg_emit_word( the_sbp, sig->dim->ndim, errp );
			for ( i=0 ; i < sig->dim->ndim ; i++ ) {
				qp_seg_emit_word( the_sbp, sig->dim->range[i], errp );
			}
		}

	} else {
		int	dims;

		if ( t->u.name.argc >= 4 ) {
			dims = 3;		/* use generic multi dim code */
		} else {
			dims = t->u.name.argc - 1;	/* use special instruction */
		}

		/* first emit code for the calculated indices (reversed ofcourse ...)*/
		for ( i=t->u.name.argc-1 ; i >= 0 ; i-- ) {
			a = &dim_list[i];
			if ( a->end == 0 )
				gen_tree( qe, a->end_tree, errp );
			if ( a->start == 0 )
				gen_tree( qe, a->start_tree, errp );
		}

		switch ( t->dim->dtyp ) {	/* switch DataType */
		case D_BOOL:	fc = FC_CWN_DYN_BOOL + 16 * dims; break;
		case D_UINT:	fc = FC_CWN_DYN_UINT + 16 * dims; break;
		case D_ULONG:	fc = FC_CWN_DYN_ULONG + 16 * dims; break;
		case D_INT:	fc = FC_CWN_DYN_INT + 16 * dims; break;
		case D_LONG:	fc = FC_CWN_DYN_LONG + 16 * dims; break;
		case D_FLOAT:	fc = FC_CWN_DYN_FLOAT + 16 * dims; break;
		case D_DOUBLE:	fc = FC_CWN_DYN_DOUBLE + 16 * dims; break;
		case D_STR:	fc = FC_CWN_DYN_STR + 16 * dims; break;
		default:
			sf_report( "gen_cvar_vec: Unkown DataType (%d)\n",
				t->val->d->dtyp );
			*errp = R_INTERNAL_ERROR;
			break;
		}
		qp_seg_emit_opc( the_sbp, fc | flags, errp );
		qp_seg_emit_word( the_sbp, index, errp );
		if ( dims == 3 ) {
			qp_seg_emit_word( the_sbp, t->u.name.argc - 1, errp );
		}

		/* dump dim_list table */
		for( i=0 ; i < t->u.name.argc ; i++ ) {
			a = &dim_list[i];
			qp_seg_emit_word( the_sbp, a->stride, errp );
			qp_seg_emit_word( the_sbp, a->max, errp );
			qp_seg_emit_word( the_sbp, a->start, errp );
			qp_seg_emit_word( the_sbp, a->end, errp );
		}
	}
}


static void
gen_rvar( QueryExe * qe, pTree t, int * errp )
{
	int	index;

	qp_assert( t->check_shape == 0 );	/* should never check a leaf node */
	qp_assert( ! t->need_shape );		/* always scalar */

	index = alloc_var( qe, Var_RWN, t->u.name.name, errp );

	qp_seg_emit_opc( the_sbp, FC_CWN_SCA_FLOAT, errp );
	qp_seg_emit_word( the_sbp, index, errp );

}


static void
gen_shape_check( QueryExe * qe, Dimension *d, int * errp )
{
	int	i;

	qp_seg_emit_opc( the_sbp, FC_CHECK_SHAPE, errp );
	qp_seg_emit_word( the_sbp, d->ndim, errp );
	for ( i=0 ; i < d->ndim ; i++ ) {
		qp_seg_emit_word( the_sbp, d->range[i], errp );
	}
}


static void
gen_fun( QueryExe * qe, pTree t, int * errp )
{
	UInt32		flags = 0;
	int		i;

	if ( t->check_shape > 0 ) {
		flags |= FC_DYNAMIC_TEST_BIT;
	}

	if ( t->need_shape ) {
		flags |= FC_DYNAMIC_INFO_BIT;
	}

	for( i=t->u.name.argc-1 ; i >= 0 ; i-- ) {
		gen_tree( qe, t->u.name.argv[i], errp );
		if ( t->u.name.check[i] ) {
			gen_shape_check(qe, t->u.name.sig->u.fun.argv[i], errp);
		}
	}

	qp_seg_emit_opc( the_sbp, t->u.name.sig->u.fun.fc | flags, errp );

	if ( t->check_shape > 0 ) {
		qp_seg_emit_word( the_sbp, t->check_shape, errp );
	}
}


static void
gen_comis( QueryExe * qe, pTree t, int * errp )
{
	Signature	*s;
	int		i, dummy;
	FCode		fc;
	char **		namelist;
	VarClass	vc;

	qp_assert( t->check_shape == 0 );	/* should never check a comis node */
	qp_assert( ! t->need_shape );		/* always scalar */

	s = t->u.name.sig;

	if ( qe->new_ntuple ) {
		vc = Var_CWN;
	} else {
		vc = Var_RWN;
	}

	namelist = s->u.comis.namelist;
	if ( namelist != 0 ) {
		for ( ; *namelist != 0 ; namelist++ ) {
			dummy = alloc_var( qe, vc, *namelist, errp );
			if ( *errp != R_NOERR ) {
				return;
			}
		}
	}

	for( i=t->u.name.argc-1 ; i >= 0 ; i-- ) {
		gen_tree( qe, t->u.name.argv[i], errp );
	}

	switch ( s->dim->dtyp ) {	/* switch DataType */
	case D_INT:
		fc = FC_CS_IFUN;
		break;
	case D_FLOAT:
		fc = FC_CS_RFUN;
		break;
	case D_DOUBLE:
		fc = FC_CS_DFUN;
		break;
	case D_BOOL:
		fc = FC_CS_LFUN;
		break;
	case D_STR:
		fc = FC_CS_SFUN;
		break;
	default:
		sf_report( "gen_comis: Function returning %s not supported\n",
			datatype_to_str( s->dim->dtyp ) );
		*errp = R_NOT_IMPLEMENTED;
		return;
		break;
	}

	qp_seg_emit_opc( the_sbp, fc, errp );
	qp_seg_emit_word( the_sbp, s->u.comis.iaddr, errp );
	qp_seg_emit_word( the_sbp, t->u.name.argc, errp );
}


static void
gen_name( QueryExe * qe, pTree t, int * errp )
{
	switch ( t->u.name.sig->typ ) {	/* switch SigType */
	case S_FUN:		/* builtin function */
		gen_fun( qe, t, errp );
		break;
	case S_CVAR:		/* cwn ntup var */
		gen_cvar_vec( qe, t, errp );
		break;
	case S_RVAR:		/* rwn ntup var */
		gen_rvar( qe, t, errp );
		break;
	case S_VEC:		/* kuip vector */
		gen_cvar_vec( qe, t, errp );
		break;
	case S_COMIS:		/* comis routine */
		gen_comis( qe, t, errp );
		break;
	case S_MASK:		/* mask */
		gen_mask_read( qe, t, errp );
		break;
	default:
		sf_report( "gen_name: Unkown SigType (%d)\n",
			t->u.name.sig->typ );
		*errp = R_INTERNAL_ERROR;
		break;
	}
}


static void
gen_cut( QueryExe * qe, pTree t, int * errp )
{
	UInt32		flags;

	qp_assert( t->check_shape == 0 );	/* should never check a cut node */

	if ( t->need_shape ) {
		flags = FC_DYNAMIC_INFO_BIT;
	} else {
		flags = 0;
	}

	qp_seg_emit_opc( the_sbp, FC_CUT | flags, errp );
	qp_seg_emit_word( the_sbp, t->u.cut.index, errp );
}


static void
gen_tree( QueryExe * qe, pTree t, int * errp )
{
	if ( t->val != 0 ) {

		gen_const( qe, t, errp );

	} else switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:	/* predefined cut */
			gen_cut( qe, t, errp );
			break;

	case N_CONST:	/* literal */
			gen_const( qe, t, errp ); /* never called from here */
			break;

	case N_OP:	/* operator (unary or binary) */
			gen_op( qe, t, errp );
			break;

	case N_NAME:	/* identifier */
			gen_name( qe, t, errp );
			break;

	case N_RANGE:	/* range */
			/*
			 *  Ranges are handled directly by the cvar/vector code
			 */
			sf_report( "qp_generate_seg: "
				"Internal error N_RANGE\n" );
			*errp = R_INTERNAL_ERROR;
			break;

	case N_MASK:	/* mask writing */
			gen_mask_write( qe, t, errp );
			break;

	default:
			sf_report( "gen_tree: unknown NodeType (%d)\n",
				t->ntyp );
			*errp = R_INTERNAL_ERROR;
			break;
	}
}



Segment
qp_generate_seg( QueryExe * qe, pTree t, int * errp )
{
	Segment		s;
	UInt32		flags;

	the_sbp = new_segbuild( errp );
	if ( *errp != R_NOERR ) {
		return 0;
	}

	gen_tree( qe, t, errp );

	if ( *errp == R_NOERR ) {

		if ( qp_flags_get( "dump" ) != 0 ) {

			if ( t->need_shape ) {
				flags = FC_DYNAMIC_INFO_BIT;
			} else {
				flags = 0;
			}

			qp_seg_emit_opc( the_sbp,
				(t->dim->dtyp<<13) | FC_DUMP | flags, errp );
		}

		/* If there are unresolved labels provide an instruction */
		/* they can resolve to 					 */
		if ( ! qp_seg_labels_resolved( the_sbp, errp ) ) {

			qp_seg_emit_opc( the_sbp, FC_NOP, errp );
		
		}

		qp_seg_flag_opc( the_sbp, FC_HALT_BIT, errp );
	
		s = segment_from_segbuild( the_sbp );

	} else {
		free_segbuild( the_sbp );
		s = 0;
	}

	return s;
}


QueryExe *
qp_generate( QueryTree * qt, int * errp )
{
	QueryExe *	qe;
	int		i;
	char		leader[16];

	qe = qp_qe_new( "qp_generate", errp );
	if ( qe == 0 ) {
		return 0;
	}

	qe->path = str_new( qt->path );
	qe->id = qt->id;

	qe->new_ntuple = HNTNEW(qe->id);

	qe->vv = vvec_new( 10 ); /* allow for 10 entries until 1st re-malloc */

	/* do selection */
	if ( qt->hasSel ) {
		/* catch the default case ... */
		if ( (qt->sel->val != 0 ) && (qt->sel->val->d->ndim == 0) &&
			(qt->sel->val->u.fval == 1.) ) {
			qe->hasSel = FALSE;
			qe->sel_size = 0;
		} else {
			qe->sel = qp_generate_seg( qe, qt->sel, errp );
			if ( *errp != R_NOERR ) {
				qp_qe_free( qe );
				return 0;
			}
			qe->hasSel = TRUE;
			qe->sel_size = dim_maxsize( qt->sel->dim );

			if ( qp_flags_get( "seg" ) != 0 ) {
				printf( "============\nSelection\n" );
				dump_segment( stdout, "sel+", qe, qe->sel );
				printf( "============\n" );
			}
		}
	}

	/* loop over expr trees */
	for ( i=0 ; i < qt->nexpr ; i++ ) {
		qe->expr[i] = qp_generate_seg( qe, qt->expr[i], errp );
		if ( *errp != R_NOERR ) {
			qp_qe_free( qe );
			return 0;
		}
		qe->expr_type[i] = qt->expr[i]->dim->dtyp;
		qe->expr_size[i] = dim_maxsize( qt->expr[i]->dim );
		qe->nexpr = i + 1;
		if ( qp_flags_get( "seg" ) != 0 ) {
			printf( "============\nExpression[%d]\n", i+1 );
			sprintf( leader, "expr%d+", i+1 );
			dump_segment( stdout, leader, qe, qe->expr[i] );
			printf( "============\n" );
		}
	}

	qe->check_shape = qt->check_shape;

	/* loop over cut trees */
	for ( i=0 ; i < qt->ncuts ; i++ ) {
		qe->cuts[i] = qp_generate_seg( qe, qt->cuts[i], errp );
		if ( *errp != R_NOERR ) {
			qp_qe_free( qe );
			return 0;
		}
		qe->cuts_type[i] = qt->cuts[i]->dim->dtyp;
		qe->cuts_size[i] = dim_maxsize( qt->cuts[i]->dim );
		qe->cuts_has_shape[i] = qt->cuts[i]->need_shape;
		qe->cuts_cid[i] = qt->cuts_cid[i];
		qe->ncuts = i + 1;
		if ( qp_flags_get( "seg" ) != 0 ) {
			printf( "============\nCut[%d]\n", i+1 );
			sprintf( leader, "cut%d+", i+1 );
			dump_segment( stdout, leader, qe, qe->cuts[i] );
			printf( "============\n" );
		}
	}

	return qe;
}
