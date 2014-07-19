/*
 *  qp_semantic.c  --
 *	Semantic checking of expression trees
 *
 *  Original:  7-Jul-1994 22:25
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_semantic.c,v 1.17 1996/10/18 07:35:04 couet Exp $
 *
 *  $Log: qp_semantic.c,v $
 *  Revision 1.17  1996/10/18 07:35:04  couet
 *  - prototype was missing
 *
 *  Revision 1.16  1996/05/06 09:18:14  maartenb
 *  - Introduce a consistent reporting of array bound errors
 *
 *  - Cleanup dynamic non scalar access.
 *
 *  Revision 1.15  1996/05/02 11:02:20  maartenb
 *  - Enabled array arguments to comis (no range checking !!)
 *
 *  Revision 1.14  1996/04/23 18:38:59  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>

#include	"cern_types.h"
#include	"errors.h"
#include	"qp_compile.h"
#include	"qp_convert.h"
#include	"qp_cuts.h"
#include	"qp_dimension.h"
#include	"qp_mask.h"
#include	"qp_report.h"
#include	"qp_semantic.h"
#include	"qp_signature.h"
#include	"qp_symtab.h"
#include	"qp_tree.h"
#include	"str.h"

extern void resolve_type_3( pTree *, pTree *, pTree *, DataType *, int *);

static bool
equal_dimension( Dimension *dim1, Dimension *dim2 )
{
	int	i;

	if ( dim1->ndim != dim2->ndim )
		return FALSE;
	
	for( i=0; i < dim1->ndim ; i++ ) {
		if ( dim1->range[i] == dim2->range[i] )
			continue;
		if ( dim1->range[i] == -1 ) /* runtime calculated range */
			continue;
		if ( dim2->range[i] == -1 ) /* runtime calculated range */
			continue;
		return FALSE;
	}

	return TRUE;
}


static pTree
sem_op( pTree t, int *r )
{
	pTree		tmp1, tmp2, tmp3;
	DataType	dt1, dt2, dt3;
	OpType		op;
	Value		*valv[MAX_DIMENSION];

	qp_assert( t->ntyp == N_OP );

	*r = R_NOERR;

	op = t->u.op.op;
	switch ( op ) {	/* switch OpType */
        case O_OR:
        case O_AND:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		t->u.op.o2 = sem_check( t->u.op.o2, r );
		if ( *r != R_NOERR ) break;
		tmp2 = t->u.op.o2;

		dt1 = tmp1->dim->dtyp;
		dt2 = tmp2->dim->dtyp;

		if ( op == O_AND &&
			( (dt1==D_BOOL && IS_NUMERIC(dt2)) ||
			(dt2==D_BOOL && IS_NUMERIC(dt1)) ) ) {

			sf_report( "Warning: boolean '.AND.' converted to '*' "
					"for backward compatibility\n" );
			t->u.op.op = O_TIMES;
			t = sem_check( t, r );
			break;
		}

		if ( tmp1->dim->dtyp != D_BOOL || tmp2->dim->dtyp != D_BOOL ) {
			sf_report( "Operands of '%s' must be of type boolean\n",
				optype_to_str( op ) );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( !equal_dimension( tmp1->dim, tmp2->dim ) &&
			tmp1->dim->ndim != 0 && tmp2->dim->ndim != 0 ) {

			sf_report( "Incompatible dimensions for operands of "
				"'%s'\n", optype_to_str( op ) );
			*r = R_TYPE_ERROR;
			break;
		}

		t->dim = dim_combine( D_UNDEF, tmp1->dim, tmp2->dim );

		if ( tmp1->val != 0 && tmp2->val != 0 ) {
			valv[0] = tmp1->val;
			valv[1] = tmp2->val;
			t->val = val_eval( optype_to_fc(op), 2, valv, r );
		}
		break;

        case O_NOT:
		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		if ( tmp1->dim->dtyp != D_BOOL ) {
			sf_report( "Operand of '.not.' must be of type "
				"boolean\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		t->dim = dim_copy( tmp1->dim );

		if ( tmp1->val != 0 ) {
			valv[0] = tmp1->val;
			t->val = val_eval( optype_to_fc(op), 1, valv, r );
		}

		break;

        case O_LT:
        case O_LE:
        case O_GT:
        case O_GE:

        case O_EQ:
        case O_NE:
        case O_CT:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		t->u.op.o2 = sem_check( t->u.op.o2, r );
		if ( *r != R_NOERR ) break;
		tmp2 = t->u.op.o2;

		dt1 = tmp1->dim->dtyp;
		dt2 = tmp2->dim->dtyp;
		if ( IS_NUMERIC( dt1 ) && IS_NUMERIC( dt2 )) {
			/* proper numerical compare */
			/* check if type promotion is needed */
			if ( dt1 != dt2 ) {
				DataType	dummy;

				resolve_type_promotion( &tmp1, &tmp2,
							&dummy, r );
			}
			if ( *r != R_NOERR )
				break;

		} else if ( dt1 == D_STR && dt2 == D_STR ) {
			/* OK -- str vs. str compare */
		} else if ( (op==O_EQ || op==O_NE) &&
			( dt1 == D_BOOL && dt2 == D_BOOL ) ) {
			/* bool vs. bool (in)equality test */
		} else {
			sf_report( "Illegal type in comparison\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( !equal_dimension( tmp1->dim, tmp2->dim ) &&
			tmp1->dim->ndim != 0 && tmp2->dim->ndim != 0 ) {

			sf_report( "Incompatible dimensions for comparison\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		t->dim = dim_combine( D_BOOL, tmp1->dim, tmp2->dim );

		/* reassign operands */

		t->u.op.o1 = tmp1;
		t->u.op.o2 = tmp2;

		if ( tmp1->val != 0 && tmp2->val != 0 ) {
			valv[0] = tmp1->val;
			valv[1] = tmp2->val;
			t->val = val_eval( optype_to_fc(op), 2, valv, r );
		}

		break;

        case O_LTLT:
        case O_LTLE:
        case O_LELT:
        case O_LELE:
        case O_GTGT:
        case O_GTGE:
        case O_GEGT:
        case O_GEGE:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		t->u.op.o2 = sem_check( t->u.op.o2, r );
		if ( *r != R_NOERR ) break;
		tmp2 = t->u.op.o2;

		t->u.op.o3 = sem_check( t->u.op.o3, r );
		if ( *r != R_NOERR ) break;
		tmp3 = t->u.op.o3;

		dt1 = tmp1->dim->dtyp;
		dt2 = tmp2->dim->dtyp;
		dt3 = tmp3->dim->dtyp;

		if ( IS_NUMERIC(dt1) && IS_NUMERIC(dt2) && IS_NUMERIC(dt3)) {

			DataType	dummy;

			resolve_type_3( &tmp1, &tmp2, &tmp3, &dummy, r );

		} else if ( dt1==D_STR && dt2==D_STR && dt3==D_STR ) {
			/* proper string compare, types are OK */
		} else {
			sf_report( "Illegal type in comparison\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( 
			( !equal_dimension( tmp1->dim, tmp2->dim ) &&
			tmp1->dim->ndim != 0 && tmp2->dim->ndim != 0 )
			||
			( !equal_dimension( tmp1->dim, tmp3->dim ) &&
			tmp1->dim->ndim != 0 && tmp3->dim->ndim != 0 )
			||
			( !equal_dimension( tmp2->dim, tmp3->dim ) &&
			tmp2->dim->ndim != 0 && tmp3->dim->ndim != 0 )
		) {

			sf_report( "Incompatible dimensions for comparison\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		t->dim = dim_combine3( D_BOOL, tmp1->dim, tmp2->dim, tmp3->dim );

		/* reassign operands */

		t->u.op.o1 = tmp1;
		t->u.op.o2 = tmp2;
		t->u.op.o3 = tmp3;

		if ( tmp1->val != 0 && tmp2->val != 0  && tmp3->val != 0) {
			valv[0] = tmp1->val;
			valv[1] = tmp2->val;
			valv[2] = tmp3->val;
			t->val = val_eval( optype_to_fc(op), 2, valv, r );
		}

		break;

        case O_PLUS:
        case O_MINUS:
        case O_TIMES:
        case O_DIV:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		t->u.op.o2 = sem_check( t->u.op.o2, r );
		if ( *r != R_NOERR ) break;
		tmp2 = t->u.op.o2;

		dt1 = tmp1->dim->dtyp;
		dt2 = tmp2->dim->dtyp;

		if ( !equal_dimension( tmp1->dim, tmp2->dim ) &&
			tmp1->dim->ndim != 0 && tmp2->dim->ndim != 0 ) {

			sf_report( "Incompatible dimensions for binary operator '%s'\n",
				optype_to_str( op ) );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( IS_NUMERIC( dt1 ) && IS_NUMERIC( dt2 )) {
			DataType	res_dtyp;

			/* check if type promotion is needed */
			if ( dt1 != dt2 ) {
				resolve_type_promotion( &tmp1, &tmp2,
							&res_dtyp, r );
			} else {
				res_dtyp = dt1;
			}
			if ( *r != R_NOERR )
				break;

			t->dim = dim_combine( res_dtyp, tmp1->dim, tmp2->dim );

		} else if ( op == O_TIMES &&
			( (dt1==D_BOOL && IS_NUMERIC(dt2)) || (dt2==D_BOOL && IS_NUMERIC(dt1)) )
		) {
			/* assure "bool * weight" order of operands */
			if ( dt2 == D_BOOL ) {
				pTree	tmp;

				tmp = tmp1; tmp1 = tmp2; tmp2 = tmp;
			}
			tmp1 = sem_check(
				new_conv_node( tmp1,
					convert_type(D_BOOL,tmp2->dim->dtyp )),
				r );
			if ( *r != R_NOERR )
				break;

			t->dim = dim_combine( tmp2->dim->dtyp,
						tmp1->dim, tmp2->dim );

		} else {
			sf_report( "Illegal type for binary operator '%s'\n",
				optype_to_str( op ) );
			*r = R_TYPE_ERROR;
			break;
		}

		/* reassign operands */

		t->u.op.o1 = tmp1;
		t->u.op.o2 = tmp2;

		if ( tmp1->val != 0 && tmp2->val != 0 ) {
			valv[0] = tmp1->val;
			valv[1] = tmp2->val;
			t->val = val_eval( optype_to_fc(op), 2, valv, r );
		}

		break;

        case O_UMINUS:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		dt1 = tmp1->dim->dtyp;

		if ( IS_NUMERIC( dt1 ) ) {
			t->dim = dim_copy( tmp1->dim );
			t->dim->isRange = FALSE;
			if ( dt1==D_UINT ) {
				t->dim->dtyp = D_INT;
			}
		} else {
			sf_report( "Illegal type for unary minus\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( tmp1->val != 0 ) {
			valv[0] = tmp1->val;
			t->val = val_eval( optype_to_fc(op), 1, valv, r );
		}

		break;

        case O_POW:

		t->u.op.o1 = sem_check( t->u.op.o1, r );
		if ( *r != R_NOERR ) break;
		tmp1 = t->u.op.o1;

		t->u.op.o2 = sem_check( t->u.op.o2, r );
		if ( *r != R_NOERR ) break;
		tmp2 = t->u.op.o2;

		dt1 = tmp1->dim->dtyp;
		dt2 = tmp2->dim->dtyp;

		if ( !equal_dimension( tmp1->dim, tmp2->dim ) &&
			tmp1->dim->ndim != 0 && tmp2->dim->ndim != 0 ) {

			sf_report( "Incompatible dimensions for power operator\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( IS_NUMERIC( dt1 ) && IS_NUMERIC( dt2 )) {

			DataType	res_dtyp;

			/* check if type promotion is needed */
			if ( dt1 != dt2 ) {
				resolve_type_promotion( &tmp1, &tmp2,
							&res_dtyp, r );
			} else {
				res_dtyp = dt1;
			}
			if ( *r != R_NOERR )
				break;

			t->dim = dim_combine( res_dtyp, tmp1->dim, tmp2->dim );

		} else {
			sf_report( "Non numeric argument(s) for power operator\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		/* reassign operands */

		t->u.op.o1 = tmp1;
		t->u.op.o2 = tmp2;

		if ( tmp1->val != 0 && tmp2->val != 0 ) {
			valv[0] = tmp1->val;
			valv[1] = tmp2->val;
			t->val = val_eval( optype_to_fc(op), 2, valv, r );
		}

		break;

	default:
		qp_abort( "sem_op: Unkown OpType %d\n", op );
		break;
	}

	return t;
}


static pTree
sem_fun( pTree t, Signature *sigs, int *r )
{
	int		i, j;
	Signature	*sig, *s;
	Dimension	*sd, *ad, *var_dim;
	Value		*vals[MAX_DIMENSION];
	bool		allConst;

	allConst = TRUE;

	if ( sigs->next != 0 ) {
		sig = 0;
		for( s = sigs; s != 0 ; s = s->next ) {
			bool	fail;

			if ( t->u.name.argc != s->u.fun.argc )
				continue;

			for( i=0, fail = FALSE ; i < t->u.name.argc ; i++ ) {
				sd = s->u.fun.argv[i];
				ad = t->u.name.argv[i]->dim;

				if (  sd->dtyp != ad->dtyp ) {
					fail = TRUE;
					break;
				}
			}

			if ( fail )
				continue;
			
			sig = s;
		}

		if ( sig == 0 ) {
			sf_report( "Arguments of '%s' do not match known"
				" signatures\n  Available are:\n", t->u.name.name );
			for( s = sigs; s != 0 ; s = s->next ) {
				fputs( "  ", stdout );
				fputs( t->u.name.name, stdout );
				fputs( "( ", stdout );
				for( i=0 ; i < s->u.fun.argc ; i++ ) {
					sd = s->u.fun.argv[i];
					if ( i != 0 )
						fputs( ", ", stdout );
					print_dimension( stdout, sd );
				}
				fputs( " )\n", stdout );
			}
			*r = R_TYPE_ERROR;
			return t;
		}

	} else {
		sig = sigs;
	}

	if ( t->u.name.argc != sig->u.fun.argc ) {
		sf_report( "Wrong number of arguments for '%s' (expect %d)\n",
			t->u.name.name, sig->u.fun.argc );
		*r = R_TYPE_ERROR;
		return t;
	}

	t->u.name.sig = sig;

	var_dim = dim_new( D_UNDEF );	/* nothing known sofar */
	var_dim->ndim = -1;	/* nothing known sofar */

	for( i=0 ; i < t->u.name.argc ; i++ ) {
		sd = sig->u.fun.argv[i];
		ad = t->u.name.argv[i]->dim;
		if (  sd->dtyp != ad->dtyp ) {
			sf_report( "Wrong basic type for argument %d of '%s'\n",
				i+1, t->u.name.name );
			*r = R_TYPE_ERROR;
			return t;
		}
		switch ( sd->ndim ) {	/* switch int */
		case 0:		/* scalar */
			if ( ad->ndim != 0 ) {
				sf_report( "Argument %d of '%s' must be scalar\n",
					i+1, t->u.name.name );
				*r = R_TYPE_ERROR;
				return t;
			}
			break;

		case -1:	/* accept any array or scalar */

/*
 *  		All type -1 arguments of a function must have an identical
 *		shape (Dimension) at run-time
 *  		At compile time do as much checking as possible 
 *  		and generate runtime check when necessary
 *  
 *  		possible cases:
 *  
 *  		-no variable arguments
 *  		-all known at compile time
 *  			(possibly scalar, fixed after loop)
 *  		-atleast one known at compile time but not all
 *  			-> known at compile time but need check for some
 *  		-none known at compile time
 */

		if ( ad->ndim < 0 ) {
			/* Should not happen, ndim is known at this stage */
			qp_abort( "sem_fun: Internal error 'ad->ndim < 0'\n" );
		} else if ( ad->ndim == 0 ) {
			/* scalar always matches */
		} else {
			if ( var_dim->ndim == -1 ) { /* first definition */
				var_dim->ndim = ad->ndim;
				for( j=0; j<var_dim->ndim ; j++ ) {
					var_dim->range[j] = ad->range[j];
					var_dim->max_range[j] =
						ad->max_range[j];
				}
			} else for( j=0; j<var_dim->ndim ; j++ ) {
				if ( var_dim->range[j] == -1 ){
					var_dim->range[j] = ad->range[j];
					if ( ad->range[j] == -1 ) {
						if ( ad->max_range[j] >
						var_dim->max_range[j] ){
							var_dim->max_range[j] =
							ad->max_range[j];
						}
					}
				} else if (ad->range[j] == -1 ) {
					/* */
				} else if (var_dim->range[j] !=
						ad->range[j]){
					sf_report( "Inconsistent"
						" dimension "
						"for argument"
						" %d\n", i );
						*r =
						R_TYPE_ERROR;
					return t;
				}
			}
		}
		break;

		default:	/* specific array shape */
			if ( ad->ndim != sd->ndim ) {
				sf_report( "Argument %d of '%s' must be %d "
					"dimensional\n", i+1, t->u.name.name,
					sd->ndim );
				*r = R_TYPE_ERROR;
				return t;
			}

			if ( !equal_dimension( sd, ad ) ) {
				sf_report( "Argument %d of '%s' has wrong "
					"dimension\n", i+1, t->u.name.name );
				*r = R_TYPE_ERROR;
				return t;
			}
			break;
		}

		if ( t->u.name.argv[i]->val == 0 ) 
			allConst = FALSE;
		
		vals[i] = t->u.name.argv[i]->val;
	}

	/* the variable dimension is scalar */
	if ( var_dim->ndim == -1 ) {
		var_dim->ndim = 0;
	}

	/* determine t->dim */

	t->dim = dim_copy( sig->dim );
	if ( sig->dim->ndim == -1 ) {
		t->dim->ndim = var_dim->ndim;
		for( i=0 ; i<var_dim->ndim ; i++ ) {
			t->dim->range[i] = var_dim->range[i];
			t->dim->max_range[i] = var_dim->max_range[i];
		}
	}

	dim_del( var_dim );

	/* constant sub expression ? */

	if ( allConst ) {
		t->val = val_eval( sig->u.fun.fc, t->u.name.argc, vals, r);
	}

	return t;
}


void
qp_sem_cmd( pTree st, int n, pTree et[], int *r )
{
	int		i, j;
	Dimension	*ad, *var_dim;


	var_dim = dim_new( D_UNDEF );	/* nothing known sofar */
	var_dim->ndim = -1;	/* nothing known sofar */

	for( i=-1 ; i < n ; i++ ) {
		if ( i == -1 ) {
			ad = st->dim;
		} else {
			ad = et[i]->dim;
		}

/*
 *  		All type -1 arguments of a function must have an identical
 *		shape (Dimension) at run-time
 *  		At compile time do as much checking as possible 
 *  		and generate runtime check when necessary
 *  
 *  		possible cases:
 *  
 *  		-no variable arguments
 *  		-all known at compile time
 *  			(possibly scalar, fixed after loop)
 *  		-atleast one known at compile time but not all
 *  			-> known at compile time but need check for some
 *  		-none known at compile time
 */

		if ( ad->ndim < 0 ) {
			/* Should not happen, ndim is known at this stage */
			qp_abort( "sem_fun: Internal error 'ad->ndim < 0'\n" );
		} else if ( ad->ndim == 0 ) {
			/* scalar always matches */
		} else {
			if ( var_dim->ndim == -1 ) { /* first definition */
				var_dim->ndim = ad->ndim;
				for( j=0; j<var_dim->ndim ; j++ ) {
					var_dim->range[j] = ad->range[j];
					var_dim->max_range[j] =
						ad->max_range[j];
				}
			} else for( j=0; j<var_dim->ndim ; j++ ) {
				if ( var_dim->range[j] == -1 ){
					var_dim->range[j] = ad->range[j];
					if ( ad->range[j] == -1 ) {
						if ( ad->max_range[j] >
						var_dim->max_range[j] ){
							var_dim->max_range[j] =
							ad->max_range[j];
						}
					}
				} else if (ad->range[j] == -1 ) {
					/* */
				} else if (var_dim->range[j] !=
						ad->range[j]){
					sf_report( "Inconsistent"
						" dimension "
						"for expression"
						" %d\n", i+1 );
						*r =
						R_TYPE_ERROR;
					return;
				}
			}
		}
	}

	/* the variable dimension is scalar */
	if ( var_dim->ndim == -1 ) {
		var_dim->ndim = 0;
	}

	dim_del( var_dim );
}


/*
 *  check_bounds --
 *      If 'from' & 'to' have static values, compare them to the proper
 *      limits based on the signature. Dynamic arrays are taken into account.
 *      Also check relatve ordering.
 *	'to' can be given as zero.
 *	If index is given as -1 we are checking the string subrange.
 */
static void
check_bounds( char * name, pTree from, pTree to, Signature *sig, int index, int *r )
{
	char		*msg, header[128];
	bool		dynamic;
	Dimension	*d;
	int		to_val, from_val, upper;

	/* only for arrays or subrange */
	qp_assert( sig->dim->ndim > 0 || (index==-1 && sig->dim->dtyp==D_STR) );

	sprintf( header, "Array bounds error: index %d of '%s'\n\t",
		index + 1, name );
	d = sig->dim;

	if ( index == -1 ) 
		upper = d->strSize;
	else
		upper = d->range[index];

	/* this dimension is dynamic */
	dynamic = (index == d->ndim-1) && (d->idx != STI_INVALID);


	if ( to != 0 )
		msg = "Range start";
	else
		msg = "Index value";

	if ( from->val != 0 ) {
		from_val = from->val->u.ival;
		if (  from_val < 1 ) {
			sf_report( "%s%s (%d) less than 1\n", header,
				msg, from_val );
			*r = R_TYPE_ERROR;
			return;
		}
		if (  dynamic ) {
			if ( from_val > upper ) {
				sf_report( "%s%s (%d) larger than maximum"
					" upperbound (%d)\n" , header,msg,
					from_val, upper );
				*r = R_TYPE_ERROR;
				return;
			}
		} else {
			if ( from_val > upper ) {
				sf_report( "%s%s (%d) larger than upperbound "
					"(%d)\n", header, msg, from_val, upper );
				*r = R_TYPE_ERROR;
				return;
			}
		}
	}

	msg = "Range end";
	if ( (to != 0) && (to->val != 0) ) {
		to_val = to->val->u.ival;
		if (  to_val < 1 ) {
			sf_report( "%s%s (%d) less than one\n", header, msg, to_val );
			*r = R_TYPE_ERROR;
			return;
		}
		if (  !dynamic && to_val > upper ) {
			sf_report( "%s%s (%d) larger than upperbound (%d)\n",
				header, msg, to_val, upper );
			*r = R_TYPE_ERROR;
			return;
		}
	}

	if ( (to != 0) && (*r==R_NOERR) && (from->val != 0) && (to->val != 0) ){
		if ( from_val > to_val ) {
			sf_report( "%sRange start (%d) larger than end (%d)\n",
				header, from_val, to_val );
			*r = R_TYPE_ERROR;
			return;
		}
	}
}


static pTree
sem_cvar( pTree t, Signature *sig, int *r )
{
	pTree	tmp;
	int	i;
	bool	dynamic;

	/* special handling of string subrange */
	if ( (sig->dim->dtyp == D_STR) && (t->u.name.range == 0)
		&& (sig->dim->ndim != 1) && (t->u.name.argc == 1) ) {
		/* turn index into subrange */
		t->u.name.range = t->u.name.argv[0];
		t->u.name.argc = 0;
	}

	/* array with no indices "a" -> "a(:,:,...)" */
	if ( (t->u.name.argc == 0) && (sig->dim->ndim != 0) ) {
		for( i=0 ; i < sig->dim->ndim ; i++ ) {
			t->u.name.argv[i] = sem_check(
				new_range_node( (pTree) 0, (pTree) 0),
				r
				);
		}
		t->u.name.argc = sig->dim->ndim;
	}

	/* argc should match ndim and each arg should be int or range */

	if ( sig->dim->ndim != t->u.name.argc ) {
		sf_report( "Wrong number of indices for %s\n",
			t->u.name.name );
		*r = R_TYPE_ERROR;
		return t;
	}

	t->dim = dim_new( sig->dim->dtyp );

	for( i=0 ; i < t->u.name.argc ; i++ ) {

		tmp = t->u.name.argv[i];

		/* this dimension is dynamic */
		dynamic = (i == t->u.name.argc-1) &&
				(sig->dim->idx != STI_INVALID);

		if ( tmp->ntyp == N_RANGE ) {
			if ( tmp->u.range.from == 0 )
				tmp->u.range.from = sem_check(
					new_con_int_node( 1 ), r );
			
			if ( tmp->u.range.to == 0 ) {
				if ( ! dynamic ){
					tmp->u.range.to = sem_check(
						new_con_int_node(
							sig->dim->range[i] ),
						r );
				} else {
					char	*name;

					name = st_entry( sf_symtab,
						sig->dim->idx )->name;
					tmp->u.range.to = sem_check(
						new_name_node( str_new(name),
							(pTree) 0, (pTree) 0 ),
						r );
				}
			}
			if ( *r != R_NOERR )
				break;

			check_bounds( t->u.name.name, tmp->u.range.from,
				tmp->u.range.to, sig, i, r );
			if ( *r != R_NOERR )
				break;

			if ( (tmp->u.range.from->val != 0) &&
						(tmp->u.range.to->val != 0)){
				
				t->dim->range[t->dim->ndim] =
					tmp->u.range.to->val->u.ival -
					tmp->u.range.from->val->u.ival + 1;
				t->dim->max_range[t->dim->ndim] =
					t->dim->range[t->dim->ndim];
			} else {
				/* runtime calculated range */
				t->dim->range[t->dim->ndim] = -1;
				t->dim->max_range[t->dim->ndim] = 
					sig->dim->max_range[i];
			}
			t->dim->ndim += 1;

		} else if ( tmp->dim->dtyp==D_UINT || tmp->dim->dtyp==D_INT ) {
			if ( tmp->dim->dtyp==D_UINT ) {
				tmp = sem_check(
					new_conv_node( tmp,
					convert_type( D_UINT, D_INT ) ),
					r );
				if ( *r != R_NOERR )
					break;
			}
			if ( tmp->dim->ndim != 0 ) {
				sf_report( "Index %d of '%s' should be scalar\n",
					i+1, t->u.name.name );
				*r = R_TYPE_ERROR;
				break;
			}

			check_bounds( t->u.name.name, tmp, 0, sig, i, r );
			if ( *r != R_NOERR )
				break;

		} else {
			sf_report( "Index %d of '%s' must be integer (range)\n",
				i+1, t->u.name.name );
			*r = R_TYPE_ERROR;
			break;
		}
		
		t->u.name.argv[i] = tmp;
	}

	/* sub-range on strings */
	if ( t->u.name.range != 0 && sig->dim->dtyp == D_STR ) {
		tmp = t->u.name.range;
		if ( tmp->ntyp == N_RANGE ) {
			if ( tmp->u.range.from == 0 )
				tmp->u.range.from = sem_check(
					new_con_int_node( 1 ), r );
			
			if ( tmp->u.range.to == 0 ) {
				tmp->u.range.to = sem_check(
					new_con_int_node(
						sig->dim->strSize ),
					r );
			}

			check_bounds( t->u.name.name, tmp->u.range.from,
				tmp->u.range.to, sig, -1, r );

		} else if ( tmp->dim->dtyp==D_UINT || tmp->dim->dtyp==D_INT ) {
			if ( tmp->dim->dtyp==D_UINT ) {
				tmp = sem_check(
					new_conv_node( tmp,
					convert_type( D_UINT, D_INT ) ),
					r );
			}
			if ( *r == R_NOERR && tmp->dim->ndim != 0 ) {
				sf_report( "Index should be scalar\n" );
				*r = R_TYPE_ERROR;
			}

			check_bounds( t->u.name.name, tmp->u.range.from, 0,
				sig, -1, r );

		} else {
			sf_report( "Sub-index of '%s' should be integer "
				"(range)\n", t->u.name.name );
			*r = R_TYPE_ERROR;
		}

	} else if ( t->u.name.range != 0 && sig->dim->dtyp != D_STR ) {
		sf_report( "Subrange only for strings\n" );
		*r = R_TYPE_ERROR;
	}

	return t;
}


static pTree
sem_rvar( pTree t, Signature *sig, int *r )
{
	if ( t->u.name.argc != 0 || t->u.name.range != 0 ) {
		sf_report( "RWN variables can only be scalar\n" );
		*r = R_TYPE_ERROR;
	}
	t->dim = dim_copy( sig->dim );

	return t;
}


static pTree
sem_mask( pTree t, Signature *sig, int *r )
{
	pTree	tmp;
	int	idx;

	if ( t->u.name.argc > 1 ) {
		sf_report( "Mask cannot have more then one index\n" );
		*r = R_TYPE_ERROR;
		return t;
	}

	if ( t->u.name.range != 0 ) {
		sf_report( "Mask cannot have sub-range\n" );
		*r = R_TYPE_ERROR;
		return t;
	}

	if ( t->u.name.argc == 0 ) {	/* no index, uint type value	*/
		t->dim = dim_new( D_UINT );
	} else {			/* 1 index, should be 1<=i<=32	*/
					/* range == bit slice ??? */
		tmp = t->u.name.argv[0];
		if ( tmp->dim->dtyp == D_UINT ) {
			tmp = sem_check(
				new_conv_node( tmp,
				convert_type( D_UINT, D_INT ) ),
				r );
			if ( *r != R_NOERR )
				return t;
			t->u.name.argv[0] = tmp;
		}

		if ( tmp->dim->dtyp != D_INT ) {
			sf_report( "Mask index should be integer\n" );
			*r = R_TYPE_ERROR;
			return t;
		}
		if ( tmp->val == 0 ) {
			sf_report( "Mask index must be constant expression\n" );
			*r = R_TYPE_ERROR;
			return t;
		}
		idx = tmp->val->u.ival;
		if ( idx < 1 || idx > 32 ) {
			sf_report( "Mask index should be in the range 1..32\n");
			*r = R_TYPE_ERROR;
			return t;
		}
		t->dim = dim_new( D_BOOL );

		if ( ! mask_bit_defined( sig->u.mask.mp, idx ) ) {
			sf_report( "Bit %d of %s is not defined\n",
				idx, sig->u.mask.mp->mname );
			*r = R_TYPE_ERROR;
			return t;
		}
	}

	return t;
}


static pTree
sem_comis( pTree t, Signature *sig, int *r )
{
	int		dim, i;
	Dimension	*sd, *ad;
	Value		*vals[MAX_DIMENSION];
	bool		allConst;
	bool		full_sig;

	allConst = TRUE;
	full_sig = sig->u.fun.argc != -1;

	if ( (t->u.name.argc != sig->u.fun.argc) && full_sig ) {
		sf_report( "Wrong number of arguments for '%s' (expect %d)\n",
			t->u.name.name, sig->u.fun.argc );
		*r = R_TYPE_ERROR;
		return t;
	}

	for( i=0 ; i < t->u.name.argc ; i++ ) {
		ad = t->u.name.argv[i]->dim;
		if ( full_sig )
			sd = sig->u.fun.argv[i];
		if ( full_sig && sd->dtyp != ad->dtyp ) {
			sf_report( "Wrong basic type for argument %d of '%s'\n",
				i+1, t->u.name.name );
			*r = R_TYPE_ERROR;
			return t;
		}
		if ( full_sig )
			dim = sd->ndim;
		else 
			dim = ad->ndim;

		switch ( dim ) {	/* switch int */
		case 0:		/* scalar */
			if ( ad->ndim != 0 ) {
				sf_report( "Argument %d of '%s' must be scalar\n",
					i+1, t->u.name.name );
				*r = R_TYPE_ERROR;
				return t;
			}
			break;
		case -1:	/* accept any array */
			break;
		default:	/* specific array shape */
			break;
		}

		if ( t->u.name.argv[i]->val == 0 ) 
			allConst = FALSE;
		
		vals[i] = t->u.name.argv[i]->val;
	}

	/* determine t->dim */

	t->dim = dim_copy( sig->dim );	/* just scalar at the moment */

	/* constant sub expression ? */

	if ( allConst ) {
		t->val = val_comis( sig, t->u.name.argc, vals, r);
	}

	return t;
}


/*
 * For checking of kuip vectors it is explicitly assumed that vectors
 * cannot be of type string
 *
 */
static pTree
sem_vec( pTree t, Signature *sig, int *r )
{
	pTree	tmp;
	int	i;

	/* array with no indices "a" -> "a(:,:,...)" */
	if ( (t->u.name.argc == 0) && (sig->dim->ndim != 0) ) {
		for( i=0 ; i < sig->dim->ndim ; i++ ) {
			t->u.name.argv[i] = sem_check(
				new_range_node( (pTree) 0, (pTree) 0),
				r
				);
		}
		t->u.name.argc = sig->dim->ndim;
	}

	/* argc should match ndim and each arg should be int or range */

	if ( sig->dim->ndim != t->u.name.argc ) {
		sf_report( "Wrong number of indices for %s\n",
			t->u.name.name );
		*r = R_TYPE_ERROR;
		return t;
	}

	t->dim = dim_new( sig->dim->dtyp );

	for( i=0 ; i < t->u.name.argc ; i++ ) {

		tmp = t->u.name.argv[i];

		if ( tmp->ntyp == N_RANGE ) {
			if ( tmp->u.range.from == 0 )
				tmp->u.range.from = sem_check(
					new_con_int_node( 1 ), r );
			if ( *r != R_NOERR )
				break;
			
			if ( tmp->u.range.to == 0 ) {
				tmp->u.range.to = sem_check(
					new_con_int_node(
						sig->dim->range[i] ), r );
			}
			if ( *r != R_NOERR )
				break;

			check_bounds( t->u.name.name, tmp->u.range.from,
				tmp->u.range.to, sig, i, r );
			if ( *r != R_NOERR )
				break;

			if ( (tmp->u.range.from->val != 0) &&
						(tmp->u.range.to->val != 0)){
				
				t->dim->range[t->dim->ndim] =
					tmp->u.range.to->val->u.ival -
					tmp->u.range.from->val->u.ival + 1;
				t->dim->max_range[t->dim->ndim] =
					t->dim->range[t->dim->ndim];
			} else {
				/* runtime calculated range */
				t->dim->range[t->dim->ndim] = -1;
				t->dim->max_range[t->dim->ndim] = 
					sig->dim->max_range[t->dim->ndim];
			}
			t->dim->ndim += 1;

		} else if ( tmp->dim->dtyp==D_UINT || tmp->dim->dtyp==D_INT ) {
			if ( tmp->dim->dtyp==D_UINT ) {
				tmp = sem_check(
					new_conv_node( tmp,
					convert_type( D_UINT, D_INT ) ),
					r );
				if ( *r != R_NOERR )
					break;
			}
			if ( tmp->dim->ndim != 0 ) {
				sf_report( "Index should be scalar\n" );
				*r = R_TYPE_ERROR;
				break;
			}

			check_bounds( t->u.name.name, tmp, 0, sig, i, r );
			if ( *r != R_NOERR )
				break;

		} else {
			sf_report( "Index should be integer or range\n" );
			*r = R_TYPE_ERROR;
			break;
		}
		
		t->u.name.argv[i] = tmp;
	}

	if ( t->u.name.range != 0 ) {
		sf_report( "Subrange only for string variables\n" );
		*r = R_TYPE_ERROR;
	}

	return t;
}


static pTree
sem_name( pTree t, int *r )
{
	STEntry		*e;
	Signature	*sig;
	int		i;

	qp_assert( t->ntyp == N_NAME );

	if ( t->u.name.idx == STI_INVALID ) {
		*r = R_NAME_UNDEF;
		return t;
	}

	/* check all arguments */

	for ( i=0 ; i < t->u.name.argc ; i++ ) {
		t->u.name.argv[i] = sem_check( t->u.name.argv[i], r );
		if ( *r != R_NOERR ) {
			return t;
		}
	}

	if ( t->u.name.range != 0 ) {
		t->u.name.range = sem_check( t->u.name.range, r );
		if ( *r != R_NOERR ) {
			return t;
		}
	}

	e = st_entry( sf_symtab, t->u.name.idx );

	sig = e->sig_list;

	if ( sig->typ != S_FUN && sig->next != 0 ) {
		sf_report( "Internal error: only polymorph builtin functions" );
		abort();
	}

	t->u.name.sig = sig;	/* Possibly updated in sem_fun */

	/* compare static info from sig with actual arg expressions */
	switch ( sig->typ ) {	/* switch SigType */
	case S_FUN:
		t = sem_fun( t, sig, r );
		break;
	case S_CVAR:
		t = sem_cvar( t, sig, r );
		break;
	case S_RVAR:
		t = sem_rvar( t, sig, r );
		break;
	case S_MASK:
		t = sem_mask( t, sig, r );
		break;
	case S_COMIS:
		t = sem_comis( t, sig, r );
		break;
	case S_VEC:
		t = sem_vec( t, sig, r );
		break;
	default:
		qp_abort( "Internal error. SigType %d unknown\n", sig->typ );
		break;
	}

	return t;
}


static pTree
sem_cut( pTree t, int *errp )
{
	pTree		ctree;
	CutType		ct;

	ct = qp_cut_type( t->u.cut.cid );

	if ( ct == C_NONE ) {
		sf_report( "Cut $%d is not defined\n", t->u.cut.cid );
		*errp = R_TYPE_ERROR;
		return t;
	}

	t->u.cut.ctyp = ct;

	if ( qp_current_cut != 0 ) {
		qp_cut_add_dep( qp_current_cut, t->u.cut.cid, errp );
	}

	if ( *errp == R_NOERR ) {
		t->u.cut.index = qp_parse_cut( t->u.cut.cid, errp );
	}

	if ( *errp == R_NOERR ) {
		ctree = qp_get_cut_tree( t->u.cut.index );
		t->dim = dim_copy( ctree->dim );
	}

	return t;
}


pTree
sem_check( pTree t, int *r )
{
	STEntry		*e;
	pTree		tmp;
	Int32		ival;

	*r = R_NOERR;

	if ( t->sem_ok )
		return t;

	switch ( t->ntyp ) {	/* switch NodeType */

	case N_CUT:	/* predefined cut */

		t = sem_cut( t, r );
		break;

	case N_CONST:	/* leaf, constant */
		break;

	case N_OP:	/* operator (unary, binary, terniary ) */
		
		t = sem_op( t, r );
		break;

	case N_NAME:	/* function or array or string with range, sorted out later */
		
		t = sem_name( t, r );	/* resolve all that start with an identifier */
		break;

	case N_RANGE:	/* range */

		if ( t->u.range.from ) {
			t->u.range.from = sem_check( t->u.range.from, r );
			if ( *r != R_NOERR ) break;
			tmp = t->u.range.from;

			if ( tmp->dim->dtyp == D_UINT && tmp->dim->ndim == 0 ) {
				tmp = sem_check(
					new_conv_node( tmp,
					convert_type( D_UINT, D_INT ) ),
					r );
				if ( *r != R_NOERR )
					break;
			}

			if ( tmp->dim->dtyp != D_INT || tmp->dim->ndim != 0 ) {
				sf_report( "Subrange start should be integer\n" );
				*r = R_TYPE_ERROR;
				break;
			}
			t->u.range.from = tmp;
		}

		if ( t->u.range.to ) {
			t->u.range.to = sem_check( t->u.range.to, r );
			if ( *r != R_NOERR ) break;
			tmp = t->u.range.to;

			if ( tmp->dim->dtyp == D_UINT && tmp->dim->ndim == 0 ) {
				tmp = sem_check(
					new_conv_node( tmp,
					convert_type( D_UINT, D_INT ) ),
					r );
				if ( *r != R_NOERR )
					break;
			}

			if ( tmp->dim->dtyp != D_INT || tmp->dim->ndim != 0 ) {
				sf_report( "Subrange end should be integer\n" );
				*r = R_TYPE_ERROR;
				break;
			}
			t->u.range.to = tmp;
		}

		break;

	case N_MASK:
		t->u.mask.o1 = sem_check( t->u.mask.o1, r );
		if ( *r != R_NOERR ) break;

		t->u.mask.index = sem_check( t->u.mask.index, r );
		if ( *r != R_NOERR ) break;
		tmp = t->u.mask.index;

		if ( t->u.mask.idx == STI_INVALID ) {
			*r = R_NAME_UNDEF;
			break;
		}

		e = st_entry( sf_symtab, t->u.mask.idx );

		qp_assert( e != 0 );

		if ( e->sig_list->typ != S_MASK ) {
			sf_report( "'%s' is not a mask", e->name );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( ! mask_writable( e->sig_list->u.mask.mp ) ) {
			sf_report( "Mask '%s' is not opened for writing\n", e->name );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( tmp->dim->dtyp == D_UINT && tmp->dim->ndim == 0 ) {
			tmp = sem_check(
				new_conv_node( tmp,
				convert_type( D_UINT, D_INT ) ),
				r );
			if ( *r != R_NOERR )
				break;
		}

		if ( tmp->dim->dtyp != D_INT || tmp->dim->ndim != 0 ) {
			sf_report( "Mask index should be scalar integer\n" );
			*r = R_TYPE_ERROR;
			break;
		}
		t->u.mask.index = tmp;

		if ( tmp->val == 0 ) {
			sf_report( "Mask index should be constant\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		ival = tmp->val->u.ival;

		if ( ival < 1 || ival > 32 ) {
			sf_report( "Mask index should be between 1 and 32\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		tmp = t->u.mask.o1;
		if ( tmp->dim->ndim != 0 || ( !IS_NUMERIC(tmp->dim->dtyp) &&
			(tmp->dim->dtyp != D_BOOL) ) ) {
			sf_report( "The expression on the left side of the >>"
				" mask writing operator must be scalar and"
				" either boolean or numeric\n" );
			*r = R_TYPE_ERROR;
			break;
		}

		if ( tmp->dim->dtyp != D_FLOAT  ) {
			tmp = sem_check(
				new_conv_node( tmp,
				convert_type( tmp->dim->dtyp, D_FLOAT ) ),
				r );
			if ( *r != R_NOERR )
				break;
		}

		t->u.mask.o1 = tmp;	/* reassign argument */

		t->dim = dim_copy( t->u.mask.o1->dim );

		break;
		
	default:

		sf_report( "sem_check: UNKNOWN NODE TYPE %d\n", t->ntyp );
		*r = R_INTERNAL_ERROR;
		break;
	}

	if ( *r == R_NOERR )
		t->sem_ok = TRUE;

	return t;
}
