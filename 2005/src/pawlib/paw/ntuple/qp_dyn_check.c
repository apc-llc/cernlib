/*
 *  qp_dyn_check.c  --
 *	Traverse a tree and determine where dynamic testing is needed
 *
 *  Original: 11-Apr-1995 16:39
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_dyn_check.c,v 1.11 1996/04/23 18:38:16 maartenb Exp $
 *
 *  $Log: qp_dyn_check.c,v $
 *  Revision 1.11  1996/04/23 18:38:16  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>

#include	"errors.h"
#include	"qp_report.h"
#include	"qp_dyn_check.h"


static void
dyn_check_tree(
	pTree t,		/* determine checking for this tree */
	bool need_shape		/* arrange for the shape of this tree */
);				/* to be left on the shape stack */


/*
 *  Keep track of the cuts we want shape info from
 *  Each of such cuts must be processed atleast *once*
 *  with "dyn_check_tree( ..., TRUE )" (see dyn_check_tree). 
 *  We process each cut once with false thereby filling the list
 *  below. After that we process the list, which might grow 
 *  during this processing but never exceeds the maximum nuber
 *  of cuts because each cut is also processed *atmost* once
 *  (see cut_need_shape). 
 */

static	int	cut_list[MAX_CUTS];
static	int	cut_list_cnt;
static	int	cut_list_ptr;


void
cut_need_shape(
	int		index	/* for this query, not cut number */
)
{
	int		i;

	for ( i=0 ; i < MAX_CUTS ; i++ ) {

		if ( i == cut_list_cnt ) {
			cut_list[i] = index;
			cut_list_cnt += 1;
			return;
		}

		if ( cut_list[i] == index ) {
			return;	/* already in the list */
		}
	}

	qp_abort( "cut_need_shape: Internal error\n" );
}


static void
dyn_check_fun(
	pTree tree,		/* determine checking for this tree */
	bool need_shape		/* arrange for the shape of this tree */
				/* to be left on the shape stack */
)
{
	pTree		t;
	bool		dyn[MAX_EXPRS];
	int		nstat = 0;	/* the number of non scalar operands */
	int		ndyn = 0;	/* the number of dynamic operands */
	int		a_stat = -1;	/* index of a non scalar operand */
	int		i;
	Signature	*sig;

	tree->need_shape = need_shape;

	sig = tree->u.name.sig;

	for ( i=0 ; i < tree->u.name.argc ; i++ ) {
		tree->u.name.check[i] = FALSE;
		t = tree->u.name.argv[i];
		/*
		 * Check if a match over multiple arg's
		 * is needed
		 */
		if ( sig->u.fun.argv[i]->ndim == -1 ) {

			dyn[i] = !dim_static( t->dim );
			if ( dyn[i] ) { ndyn += 1; }
			if ( (t->dim->ndim != 0) && !dyn[i] ) {
				nstat += 1;
				a_stat = i;
			}

		} else {	/* does this single argument need checking ? */
			dyn[i] = !dim_static( t->dim );
			if ( dyn[i] ) {	/* register for single check */
				tree->u.name.check[i] = TRUE;
			}
		}
	}

	if ( (ndyn > 0) && ((ndyn+nstat) > 1) ) {
		tree->check_shape = ndyn;
		if ( nstat > 0 ) 
			tree->check_shape += 1;
	}

	if ( !need_shape && (tree->check_shape == 0) )
		a_stat = -1;


	for ( i=0 ; i < tree->u.name.argc ; i++ ) {
		t = tree->u.name.argv[i];
		dyn_check_tree( t, dyn[i] || (a_stat==i) );
	}
}


static void
dyn_check_name(
	pTree t,		/* determine checking for this tree */
	bool need_shape		/* arrange for the shape of this tree */
				/* to be left on the shape stack */
)
{
	Signature	*sig = t->u.name.sig;

	switch ( sig->typ ) {	/* switch SigType */
	case S_FUN:
		dyn_check_fun( t, need_shape );
		break;
	case S_CVAR:
		t->need_shape = need_shape;
		break;
	case S_RVAR:
		/* always scalar */
		break;
	case S_MASK:
		/* always scalar */
		break;
	case S_COMIS:
		/* always scalar, no matching of arguments ... */
		break;
	case S_VEC:
		t->need_shape = need_shape;
		break;
	default:
		qp_abort( "Internal error. SigType %d unknown\n", sig->typ );
		break;
	}
}


static void
dyn_check_op(
	pTree t,		/* determine checking for this tree */
	bool need_shape		/* arrange for the shape of this tree */
				/* to be left on the shape stack */
)
{
	pTree		t1, t2, t3;
	bool		t1_dyn, t2_dyn, t3_dyn;
	OpType		op;
	int		nstat = 0;	/* the number of non scalar operands */
	int		ndyn = 0;	/* the number of dynamic operands */
	int		a_stat = 0;	/* index of a non scalar operand */


	op = t->u.op.op;
	switch ( op ) {	/* switch OpType */

        case O_NOT:
        case O_UMINUS:
			/* never any checking to be done */
			/* delegate request for shape info */
			dyn_check_tree( t->u.op.o1, need_shape );
			break;

        case O_OR:
        case O_AND:
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
			t1 = t->u.op.o1;
			t1_dyn = ! dim_static( t1->dim );
			if ( t1_dyn ) { ndyn += 1; }
			if ( (t1->dim->ndim != 0) && !t1_dyn ) {
				nstat += 1;
				a_stat = 1;
			}

			t2 = t->u.op.o2;
			t2_dyn = ! dim_static( t2->dim );
			if ( t2_dyn ) { ndyn += 1; }
			if ( (t2->dim->ndim != 0) && !t2_dyn ) {
				nstat += 1;
				a_stat = 2;
			}

			t->need_shape = need_shape;

			if ( (ndyn > 0) && ((ndyn+nstat) > 1) ) {
				t->check_shape = ndyn;
				if ( nstat > 0 ) 
					t->check_shape += 1;
			}

			if ( !need_shape && (t->check_shape == 0) )
				a_stat = 0;

			dyn_check_tree( t1,
				t1_dyn || (a_stat==1) );

			dyn_check_tree( t2,
				t2_dyn || (a_stat==2) );

			break;
        case O_LTLT:
        case O_LTLE:
        case O_LELT:
        case O_LELE:
        case O_GTGT:
        case O_GTGE:
        case O_GEGT:
        case O_GEGE:
			t1 = t->u.op.o1;
			t1_dyn = ! dim_static( t1->dim );
			if ( t1_dyn ) { ndyn += 1; }
			if ( (t1->dim->ndim != 0) && !t1_dyn ) {
				nstat += 1;
				a_stat = 1;
			}

			t2 = t->u.op.o2;
			t2_dyn = ! dim_static( t2->dim );
			if ( t2_dyn ) { ndyn += 1; }
			if ( (t2->dim->ndim != 0) && !t2_dyn ) {
				nstat += 1;
				a_stat = 2;
			}

			t3 = t->u.op.o3;
			t3_dyn = ! dim_static( t3->dim );
			if ( t3_dyn ) { ndyn += 1; }
			if ( (t3->dim->ndim != 0) && !t3_dyn ) {
				nstat += 1;
				a_stat = 3;
			}

			t->need_shape = need_shape;

			if ( (ndyn > 0) && ((ndyn+nstat) > 1) ) {
				t->check_shape = ndyn;
				if ( nstat > 0 ) 
					t->check_shape += 1;
			}

			if ( !need_shape && (t->check_shape == 0) )
				a_stat = 0;

			dyn_check_tree( t1,
				t1_dyn || (a_stat==1) );

			dyn_check_tree( t2,
				t2_dyn || (a_stat==2) );

			dyn_check_tree( t3,
				t3_dyn || (a_stat==3) );

			break;

	default:
		qp_abort( "dyn_check_op: Internal error. Unknown operator %d\n",
			op );
		break;
	}
}


static void
dyn_check_tree(
	pTree t,		/* determine checking for this tree */
	bool need_shape		/* arrange for the shape of this tree */
				/* to be left on the shape stack */
)
{
	switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:		/* predefined cut */
			t->need_shape = need_shape;
			if ( need_shape ) {
				cut_need_shape( t->u.cut.index );
			}
			break;

	case N_CONST:	/* leaf, constant */
			t->need_shape = need_shape;
			break;

	case N_OP:		/* operator (unary or binary) */
			dyn_check_op( t, need_shape );
			break;

	case N_NAME:	/* identifier, sorted out later */
			dyn_check_name( t, need_shape );
			break;

	case N_RANGE:	/* range */
			if ( need_shape ) {	/* this should not happen */
				qp_abort( "dyn_check_tree: Internal error."
					" N_RANGE && need_shape\n" );
			}
			break;

	case N_MASK:	/* mask writing */
			break;

	default:
			qp_abort( "dyn_check_tree: Unknown NodeType %d\n", t->ntyp );
			break;
	}
}


void
qp_dyn_check( QueryTree *qt )
{
	pTree		t;
	int		i;
	bool		sel_dyn, e_dyn[MAX_EXPRS];
	int		nstat = 0;	/* the number of non scalar operands */
	int		ndyn = 0;	/* the number of dynamic operands */
	int		a_stat = -2;	/* index of a non scalar operand */

	int		check_shape = 0; /* Number of frames to check at toplevel */

	/* init cut flagging */
	cut_list_cnt = 0;
	cut_list_ptr = 0;


	if ( qt->hasSel ) {
		t = qt->sel;
		sel_dyn = ! dim_static( t->dim );
		if ( sel_dyn ) { ndyn += 1; }
		if ( (t->dim->ndim != 0) && !sel_dyn ) {
			nstat += 1;
			a_stat = -1;
		}
	}

	for ( i=0 ; i < qt->nexpr ; i++ ) {
		t = qt->expr[i];
		e_dyn[i] = ! dim_static( t->dim );
		if ( e_dyn[i] ) { ndyn += 1; }
		if ( (t->dim->ndim != 0) && !e_dyn[i] ) {
			nstat += 1;
			a_stat = i;
		}
	}

	if ( (ndyn > 0) && ((ndyn+nstat) > 1) ) {
		check_shape = ndyn;
		if ( nstat > 0 ) 
			check_shape += 1;
	}

	if ( check_shape == 0 )
		a_stat = -2;

	if ( qt->hasSel ) {
		dyn_check_tree( qt->sel,
			sel_dyn || (a_stat==-1) );
	}

	for ( i=0 ; i < qt->nexpr ; i++ ) {
		dyn_check_tree( qt->expr[i],
			e_dyn[i] || (a_stat==i) );
	}

	qt->check_shape = check_shape;		/* will be passed to interpreter */

	/*
	 *  Processing of the CUT trees
	 */

	for ( i=0 ; i < qt->ncuts ; i++ ) {
		dyn_check_tree( qt->cuts[i], FALSE );
	}

	while( cut_list_ptr < cut_list_cnt ) {
		dyn_check_tree( qt->cuts[cut_list[cut_list_ptr]], TRUE );
		cut_list_ptr += 1;
	}

	/* done ! */
}


void
qp_dyn_list( bool need_shape, int n, pTree tl[], int * ncheck )
{
	int		i;
	bool		*e_dyn;
	int		nstat = 0;	/* the number of non scalar operands */
	int		ndyn = 0;	/* the number of dynamic operands */
	int		a_stat = -1;	/* index of a non scalar operand */
	int		check_shape = 0; /* Number of frames to check at toplevel */
	pTree		t;

	e_dyn = (bool *) calloc( sizeof(bool), n > 0 ? n : 1 ); qp_assert( e_dyn != 0 );

	for ( i=0 ; i < n ; i++ ) {
		t = tl[i];
		e_dyn[i] = ! dim_static( t->dim );
		if ( e_dyn[i] ) { ndyn += 1; }
		if ( (t->dim->ndim != 0) && !e_dyn[i] ) {
			nstat += 1;
			a_stat = i;
		}
	}

	if ( (ndyn > 0) && ((ndyn+nstat) > 1) ) {
		check_shape = ndyn;
		if ( nstat > 0 ) 
			check_shape += 1;
	}

	if ( (check_shape == 0) && !need_shape )
		a_stat = -1;

	for ( i=0 ; i < n ; i++ ) {
		dyn_check_tree( tl[i],
			e_dyn[i] || (a_stat==i) );
	}

	free( e_dyn );

	*ncheck = check_shape;
}
