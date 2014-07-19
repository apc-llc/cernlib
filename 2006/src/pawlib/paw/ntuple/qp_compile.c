/*
 *  qp_compile.c  --
 *	The compiler driver
 *
 *  Original:  4-Jan-1995 18:24
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_compile.c,v 1.18 1997/06/13 13:34:42 couet Exp $
 *
 *  $Log: qp_compile.c,v $
 *  Revision 1.18  1997/06/13 13:34:42  couet
 *  - bug fixed in qp_parse_cut: on OSF graphical cuts on one variable crashes PAW
 *    in commands like: nt/plot 30.x $1 where $1 is a graphical cut on 1 variable.
 *
 *  Revision 1.17  1996/10/18 13:54:51  couet
 *  - more prototype
 *
 *  Revision 1.16  1996/05/14 12:23:26  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.15  1996/04/23 18:38:11  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<string.h>

#include	"cern_types.h"
#include	"errors.h"
#include	"qpflags.h"
#include	"mstr.h"
#include	"qp_report.h"
#include	"str.h"
#include	"qp_compile.h"
#include	"qp_dyn_check.h"
#include	"qp_generate.h"
#include	"qp_semantic.h"
#include	"qp_st_init.h"
#include	"qp_symtab.h"
#include	"qp_tree.h"


QuerySrc	*the_qs_record;
QueryTree	*the_qt_record;
int		 qp_current_cut;

void qp_scanner_set_input( char * );


static pTree
parse_expr( QuerySrc * qs, String expr, int * errp )
{
	pTree	t;

	mstr_reset();
	reset_alloc_list();
	qp_scanner_set_input( expr );

	tag_alloc_flag = TRUE;
	*errp = qp_parser_parse() == 0 ? R_NOERR : R_SYNTAX_ERROR;
	tag_alloc_flag = FALSE;

	if ( *errp == R_NOERR ) {
		clear_alloc_list();
	} else {
		free_alloc_list();
		mstr_del_all();
		return 0;
	}

	t = sem_check( qp_the_parse_tree, errp );

	if ( *errp != R_NOERR ) {
		free_tree( t );
		return 0;
	}

	return t;
}


pTree
qp_get_cut_tree( int idx )
{
	QueryTree	*qt = the_qt_record;

	qp_assert(  (0 <= idx) && (idx < qt->ncuts) && (qt->cuts[idx] != 0) );

	return qt->cuts[idx];
}


int
qp_parse_cut( int cid, int *errp )
{
	QuerySrc	*qs = the_qs_record;
	QueryTree	*qt = the_qt_record;
	int		save_current_cut, idx, i;
	char		*expr;

	for ( i=0 ; i < qs->ncuts ; i++ ) {
		if ( cid == qs->cuts[i] ) {
			return i;
		}
	}

	if ( qs->ncuts == MAX_CUTS ) {
		*errp = R_ALLOC_ERROR;
		sf_report( "Maximum number of cuts (%d) in a query exceeded\n",
			MAX_CUTS );
		return -1;
	}

	idx = qs->ncuts;
	qs->cuts[qs->ncuts++] = cid;

	save_current_cut = qp_current_cut;
	qp_current_cut = cid;

	if ( qp_cut_type( cid ) == C_GRAF ) {
		char	*x, *y;

		qp_gcut_expr( cid, &x, &y );

		if ( y == 0 ) {
		        expr = str_alloc( strlen(x) + 40 );
			sprintf( expr, "qp_gcut_1d(%d,real(%s))", idx, x );
		} else {
		        expr = str_alloc( strlen(x) + strlen(y) + 40 );
			sprintf( expr, "qp_gcut_2d(%d,real(%s),real(%s))", idx, x, y );
		}

	} else {
		expr = str_new( qp_cut_expr( cid ) );
	}

	qt->cuts[idx] = parse_expr( qs, expr, errp );
	qt->cuts_cid[idx] = cid;
	if ( *errp != R_NOERR ) {
		qp_qt_free( qt );
		qp_current_cut = save_current_cut;
		return -1;
	}

	if ( idx + 1 > qt->ncuts ) {	/* update ncuts if necessary */
		qt->ncuts = idx + 1;
	}

	qp_current_cut = save_current_cut;
	str_del( expr );

	return idx;
}


static QueryTree *
qp_parse( QuerySrc * qs, bool match_expr, int * errp )
{
	ConvType	conv;
	DataType	dtype;
	QueryTree *	qt;
	int		i;


	qt = qp_qt_new( "qp_parse", errp );
	if ( qt == 0 ) {
		return 0;
	}

	qt->path = str_new( qs->path );
	qt->id = qs->id;

	the_qs_record = qs;
	the_qt_record = qt;
	qp_current_cut = 0;

	if ( qs->hasSel ) {
		qt->sel = parse_expr( qs, qs->sel, errp );
		if ( *errp != R_NOERR ) {
			qp_qt_free( qt );
			return 0;
		}

		/*
		 * Selection is always converted to float.
		 *
		 */
		
		dtype = qt->sel->dim->dtyp;
		if ( dtype == D_STR ) {
			sf_report( "Selection or weigth cannot be of type "
				"string\n" );
			*errp = R_TYPE_ERROR;
			qp_qt_free( qt );
			return 0;
		} else if ( dtype != D_FLOAT ) {
			conv = convert_type( dtype, D_FLOAT );
			qt->sel = sem_check(
				new_conv_node( qt->sel, conv ),
				errp );
		}

		if ( *errp != R_NOERR ) {
			qp_qt_free( qt );
			return 0;
		}

		qt->hasSel = TRUE;
	}

	for ( i=0 ; i < qs->nexpr ; i++ ) {
		qt->expr[i] = parse_expr( qs, qs->expr[i], errp );
		if ( *errp != R_NOERR ) {
			qp_qt_free( qt );
			return 0;
		}
		qt->nexpr = i + 1;
	}

	/* cuts are processed recursively during parse_expr's */

	/* Compile time check of matching of expression dimensions */	
	if ( match_expr ) {
		qp_sem_cmd( qt->sel, qt->nexpr, qt->expr, errp );
		if ( *errp != R_NOERR ) {
			qp_qt_free( qt );
			return 0;
		}
	}

	qp_dyn_check( qt );

	if ( qp_flags_get( "tree" ) != 0 ) {

		if ( qs->hasSel ) {
			printf( "------------\nSelection: %s\n", qs->sel );
			dump_tree( stdout, 0, qt->sel );
			printf( "------------\n" );
		}

		for ( i=0 ; i < qs->nexpr ; i++ ) {
			printf( "------------\nExpression[%d]: %s\n",
				i+1, qs->expr[i] );
			dump_tree( stdout, 0, qt->expr[i] );
			printf( "------------\n" );
		}

		for ( i=0 ; i < qs->ncuts ; i++ ) {
			printf( "------------\nCut[%d]: %s\n",
				i+1, qp_cut_expr(qs->cuts[i]) );
			dump_tree( stdout, 0, qt->cuts[i] );
			printf( "------------\n" );
		}
	}

	return qt;
}


QueryExe *
qp_compile( QuerySrc * qs, bool match_expr, int * errp )
{
	QueryTree	*qt;
	QueryExe	*qe = 0;

	if ( sf_symtab == 0 ) {
		qp_symtab_init( errp );
		if ( *errp != R_NOERR )
			return 0;
	}

	qt = qp_parse( qs, match_expr, errp );

/* register ntuple names ?? */

	if ( *errp == R_NOERR ) {

		qe = qp_generate( qt, errp );

		if ( *errp == R_NOERR ) {
			int	i;

			if ( qe->hasSel ) {
				qe->sel_str = str_new( qs->sel );
			}

			for ( i=0 ; i < qe->nexpr ; i++ ) {
				qe->expr_str[i] = str_new( qs->expr[i] );
			}
		}

		qp_qt_free( qt );

	}

	st_sweep( sf_symtab );	/* clear all temporary symbols */

	return qe;
}
