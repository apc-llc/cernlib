/*
 *  qp_tree.c  -  implement the tree handling routines
 *
 *  Original: 19-Jun-1994 19:32
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_tree.c,v 1.10 1996/05/15 13:11:00 maartenb Exp $
 *
 *  $Log: qp_tree.c,v $
 *  Revision 1.10  1996/05/15 13:11:00  maartenb
 *  - Fix handling of partial leftover datastructures.
 *
 *  Revision 1.9  1996/04/23 18:39:05  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>

#include	"qp_dimension.h"
#include	"qp_name.h"
#include	"qp_report.h"
#include	"qp_symtab.h"
#include	"qp_tree.h"
#include	"str.h"


static pTree	alloc_list = 0;
bool		tag_alloc_flag = FALSE;


static char	*_nodetype_to_str[] = {
	0,
	"Cut",		/* predefined cut */
	"Const",	/* leaf, constant */
	"Op",		/* operator (unary or binary) */
	"Name",		/* name with 0 or more args, optional subrange*/
	"Range",	/* range */
	"Mask"		/* Mask output */
};


char *
nodetype_to_str( const NodeType n )
{
	qp_assert( 0 < n && n < ((sizeof(_nodetype_to_str)/sizeof(char *))) );

	return _nodetype_to_str[n] ;
}


static char	*_optype_to_str[] = {
	".or.",
	".and.",
	"!",
	"<",
	"<=",
	"ltlt",
	"ltle",
	"lelt",
	"lele",
	">",
	">=",
	"gtgt",
	"gtge",
	"gegt",
	"gege",
	"=",
	"<>",
	"#",
	"+",
	"-",
	"*",
	"/",
	"-",
	"^",
	"vadd",
	"vmul",
	"vand",
	"vor",
	"vmin",
	"vmax",
	"min",
	"max"
};


char *
optype_to_str( const OpType o )
{
	qp_assert( 0 <= o && o < ((sizeof(_optype_to_str)/sizeof(char *))) );

	return _optype_to_str[o] ;
}


static FCode _optype_to_fc[] = {
	FC_OR,
	FC_AND,
	FC_NOT,

	FC_LT,
	FC_LE,

	FC_LTLT,
	FC_LTLE,
	FC_LELT,
	FC_LELE,

	FC_GT,
	FC_GE,

	FC_GTGT,
	FC_GTGE,
	FC_GEGT,
	FC_GEGE,

	FC_EQ,
	FC_NE,
	FC_CT,

	FC_PLUS,
	FC_MINUS,
	FC_TIMES,
	FC_DIV,

	FC_UMINUS,

	FC_POW,

	FC_VADD,
	FC_VMUL,
	FC_VAND,
	FC_VOR,
	FC_VMIN,
	FC_VMAX,

	FC_MIN,
	FC_MAX
};


FCode
optype_to_fc( const OpType o )
{
	qp_assert( 0 <= o && o < ((sizeof(_optype_to_fc)/sizeof(FCode))) );

	return _optype_to_fc[o] ;
}


OpType
fc_to_optype( const FCode fc )
{
	OpType		o;

	for ( o=0 ; o < (sizeof(_optype_to_fc)/sizeof(FCode)) ; o++ ) {
		if ( _optype_to_fc[o] == (fc & ((1<<CAT_BITS) - 1)) ) {
			return o;
		}
	}

	qp_abort( "fc_to_optype: Internal error\n" );
	/* NOTREACHED */
}


static void
spaces( FILE * fp, int n ) {
	register int i;

	for( i=4*n; i > 0 ; i-- )
		fputc( ' ', fp );
}


static pTree
new_node( void )
{
	pTree	t;

	t = (pTree) calloc( 1, sizeof(Tree) );

	if ( t == 0 ) {
		qp_abort( "Cannot allocate memory for 'Tree' struct.\n" );
	}

	if ( tag_alloc_flag ) {	/* only tag during parsing */
		t->next_alloc = alloc_list;
		alloc_list = t;
	}

	return t;
}


void
reset_alloc_list( void )
{
	if ( alloc_list != 0 ) {
		sf_report("Warning: internal inconsistency. Leaking memory\n"
			"\tthis could result from earlier problems\n"
			"\tPlease restart PAW after saving your data\n" );
		alloc_list = 0;
	}
}


void
clear_alloc_list( void )
{
	pTree t1, t2;

	t1 = alloc_list;

	while( t1 ) {
		t2 = t1;
		t1 = t1->next_alloc;
		t2->next_alloc = 0;
	}

	alloc_list = 0;
}


void
free_alloc_list( void )
{
	pTree t1, t2;

	t1 = alloc_list;

	while( t1 ) {
		t2 = t1;
		t1 = t1->next_alloc;
		free_node( t2 );
	}

	alloc_list = 0;
}


/*
 *  Create operator nodes
 */

pTree
new_op_node( const OpType op, const pTree o1, const pTree o2, const pTree o3 )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_OP;

	t->u.op.op = op;
	t->u.op.o1 = o1;
	t->u.op.o2 = o2;
	t->u.op.o3 = o3;

	return t;
}


/*
 *  Create literal nodes
 */

pTree
new_con_bool_node( const bool bval )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_BOOL );

	t->val = val_new_bool( bval );

 	return t;
}


pTree
new_con_uint_node( const unsigned int uval )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_UINT );

	t->val = val_new_uint( uval );

	return t;
}


pTree
new_con_int_node( const int ival )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_INT );

	t->val = val_new_int( ival );

	return t;
}


pTree
new_con_float_node( const double lfval )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_FLOAT );

	t->val = val_new_float( lfval );

	return t;
}


pTree
new_con_double_node( const double lfval )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_DOUBLE );

	t->val = val_new_double( lfval );

	return t;
}


pTree
new_con_str_node( char *const sval )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CONST;

	t->dim = dim_new( D_STR );
	t->dim->strSize = strlen( sval );

	t->val = val_new_str( sval );

	return t;
}


/*
 *  Create cut nodes
 */

pTree
new_cut_node( const int cid )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_CUT;

	t->u.cut.cid = cid;
	t->u.cut.ctyp = C_NONE;

	return t;
}


/*
 *  Create node for indentifier
 */

pTree
new_name_node( char *const name, const pTree arg_list, const pTree sub_range )
{
	pTree	t, u, tmp[MAX_DIMENSION];
	int	i, j, r;
	STIndex	idx;

	for( i=0, u=arg_list; (i<MAX_DIMENSION) && u ; i++, u = u->next )
		tmp[i] = u;

	if ( i == MAX_DIMENSION && u != 0 ) {
		sf_report( "new_name_node: '%s' has more then %d arguments\n",
			name, MAX_DIMENSION );
	}

	t = new_node();

	t->ntyp = N_NAME;

	t->u.name.name = name;

	t->u.name.argc = i;
	for ( j=0; i> 0 ; j++, i-- ) {
		t->u.name.argv[j] = tmp[i-1];
		tmp[i-1]->next = 0;
	}

	t->u.name.range = sub_range;

	idx = st_index( sf_symtab, name );
	if ( idx == STI_INVALID ) {
		idx = name_resolve( name, &r );
	}
	t->u.name.idx = idx;

	return t;
}


pTree
new_range_node( const pTree from, const pTree to )
{
	pTree	t;

	t = new_node();

	t->ntyp = N_RANGE;

	t->u.range.from = from;
	t->u.range.to = to;

	return t;
}


pTree
new_mask_node( const pTree o1, char * const name, const pTree index )
{
	pTree	t;
	STIndex	idx;
	int	r;

	t = new_node();

	t->ntyp = N_MASK;

	t->u.mask.o1 = o1;
	t->u.mask.name = name;

	idx = st_index( sf_symtab, name );
	if ( idx == STI_INVALID ) {
		idx = name_resolve( name, &r );
	}
	t->u.mask.idx = idx;

	t->u.mask.index = index;

	return t;
}


/*
 *  Create type conversion nodes
 */

pTree
new_conv_node( const pTree t, const ConvType c )
{
	/* -- check for repeated converts -- ? */

	return new_name_node(
		str_new( convtype_to_str( c )),
		t, (pTree) 0 );
}


/*
 *  
 *  Routines for printing a parse tree
 *  
 */


void
print_name_node( FILE* const fp, const pTree t )
{
	int	i;

	fprintf( fp, "%s", t->u.name.name );

	if ( t->u.name.argc > 0 )
		fputc( '(', fp );

	for( i=0; i < t->u.name.argc ; i++ ) {
		if ( i > 0 ) fputc( ',', fp );
		print_tree( fp, t->u.name.argv[i] );
	}

	if ( t->u.name.argc > 0 )
		fputc( ')', fp );
	
	if ( t->u.name.range ) {
		fputc( '(', fp );
		print_tree( fp, t->u.name.range );
		fputc( ')', fp );
	}
}


void
print_const_node( FILE* const fp, const pTree t )
{
	val_print( fp, t->val );
}


void
print_op_node( FILE* const fp, const pTree t )
{
	char	*op1, *op2;

	switch( t->u.op.op ) {	/* switch OpType */
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
		print_tree( fp, t->u.op.o1 );
		fputs( _optype_to_str[t->u.op.op], fp );
		print_tree( fp, t->u.op.o2 );
		break;
	case O_NOT:
	case O_UMINUS:
		fputs( _optype_to_str[t->u.op.op], fp );
		print_tree( fp, t->u.op.o1 );
		break;
	case O_LTLT:
	case O_LTLE:
	case O_LELT:
	case O_LELE:
	case O_GTGT:
	case O_GTGE:
	case O_GEGT:
	case O_GEGE:
		switch ( t->u.op.op ) {	/* switch OpType */
		case O_LTLT: op1 = "<"; op2 = "<"; break;
		case O_LTLE: op1 = "<"; op2 = "<="; break;
		case O_LELT: op1 = "<="; op2 = "<"; break;
		case O_LELE: op1 = "<="; op2 = "<="; break;
		case O_GTGT: op1 = ">"; op2 = ">"; break;
		case O_GTGE: op1 = ">"; op2 = ">="; break;
		case O_GEGT: op1 = ">="; op2 = ">"; break;
		case O_GEGE: op1 = ">="; op2 = ">="; break;
		}
		print_tree( fp, t->u.op.o1 );
		fputs( op1, fp );
		print_tree( fp, t->u.op.o2 );
		fputs( op2, fp );
		print_tree( fp, t->u.op.o3 );
		break;
	default:
		qp_abort( "print_op_node: Unkown OpType %d\n", t->u.op.op );
		break;
	}
}


void
print_tree( FILE * const fp, const pTree t )
{

	switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:		/* predefined cut */
			fprintf( fp, "$%d", t->u.cut.cid );
			break;
	case N_CONST:	/* leaf, constant */
			print_const_node( fp, t );
			break;
	case N_OP:		/* operator (unary or binary) */
			fputc( '(', fp );
			print_op_node( fp, t );
			fputc( ')', fp );
			break;
	case N_NAME:	/* function or array or string with range, sorted out later */
			print_name_node( fp, t );
			break;
	case N_RANGE:	/* range */
			if ( t->u.range.from ) {
				fputc( '(', fp );
				print_tree( fp, t->u.range.from );
				fputc( ')', fp );
			}
			fputc( ':', fp );
			if ( t->u.range.to ) {
				fputc( '(', fp );
				print_tree( fp, t->u.range.to );
				fputc( ')', fp );
			}
			break;
	case N_MASK:	/* mask writing */
			print_tree( fp, t->u.mask.o1 );
			fprintf( fp, ">>%s(", t->u.mask.name );
			print_tree( fp, t->u.mask.index );
			fputc( ')', fp );
			break;
	default:
			qp_abort( "print_tree: Unkown NodeType %d\n", t->ntyp );
			break;
	}

}


/*
 *  Free all memory associated with a parse tree
 */

void
free_tree( const pTree t )
{

	switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:		/* predefined cut */
			break;
	case N_CONST:	/* leaf, constant */
			break;
	case N_OP:		/* operator (unary, binary, ternary) */
			free_tree( t->u.op.o1 );
			if ( t->u.op.o2 )
				free_tree( t->u.op.o2 );
			if ( t->u.op.o3 )
				free_tree( t->u.op.o3 );
			break;
	case N_NAME:	/* function or array or string var with range, sorted out later */
			{
				int	i;

				free( (void *) t->u.name.name );

				for ( i=0; i < t->u.name.argc ; i++ ) {
					free_tree( t->u.name.argv[i] );
				}

				if ( t->u.name.range ) {
					free_tree( t->u.name.range );
				}

				break;
			}
	case N_MASK:
			free_tree( t->u.mask.o1 );
			free( (void *) t->u.mask.name );
			free_tree( t->u.mask.index );
			break;
	case N_RANGE:	/* range */
			if ( t->u.range.from )
				free_tree( t->u.range.from );
			if ( t->u.range.to )
				free_tree( t->u.range.to );
			break;
	default:
			qp_abort( "free_tree: Unkown NodeType %d\n", t->ntyp );
			break;
	}

	if ( t->dim ) {
		dim_del( t->dim );
	}

	if ( t->val ) {
		val_del( t->val );
	}

	free( (void *) t );
}


/*
 *  Free all memory associated with one node
 *  used for cleanup after syntax error.
 */

void
free_node( const pTree t )
{

	switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:	/* predefined cut */
			break;
	case N_CONST:	/* leaf, constant */
			break;
	case N_OP:	/* operator (unary, binary, terniary) */
			break;
	case N_NAME:	/* function, array, string+range, sorted out later */
			free( (void *) t->u.name.name );
			break;
	case N_RANGE:	/* range */
			break;
	case N_MASK:	/* mask writing */
			free( (void *) t->u.mask.name );
			break;
	default:
			qp_abort( "free_node: Unkown NodeType %d\n", t->ntyp );
			break;
	}

	if ( t->dim ) {
		dim_del( t->dim );
	}

	if ( t->val ) {
		val_del( t->val );
	}

	free( (void *) t );
}


static void
dump_op_node( FILE* const fp, int lvl, const pTree t )
{
	switch( t->u.op.op ) {	/* switch OpType */
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
		spaces( fp, lvl );
		fputs( _optype_to_str[t->u.op.op], fp );
		fputc( '\n', fp );
		dump_tree( fp, lvl+1, t->u.op.o1 );
		dump_tree( fp, lvl+1, t->u.op.o2 );
		break;
	case O_NOT:
	case O_UMINUS:
		spaces( fp, lvl );
		fputs( _optype_to_str[t->u.op.op], fp );
		fputc( '\n', fp );
		dump_tree( fp, lvl+1, t->u.op.o1 );
		break;
	case O_LTLT:
	case O_LTLE:
	case O_LELT:
	case O_LELE:
	case O_GTGT:
	case O_GTGE:
	case O_GEGT:
	case O_GEGE:
		spaces( fp, lvl );
		switch ( t->u.op.op ) {	/* switch OpType */
		case O_LTLT: fputs( "< <\n", fp ); break;
		case O_LTLE: fputs( "< <=\n", fp ); break;
		case O_LELT: fputs( "<= <\n", fp ); break;
		case O_LELE: fputs( "<= <=\n", fp ); break;
		case O_GTGT: fputs( "> >\n", fp ); break;
		case O_GTGE: fputs( "> >=\n", fp ); break;
		case O_GEGT: fputs( ">= >\n", fp ); break;
		case O_GEGE: fputs( ">= >=\n", fp ); break;
		}
		dump_tree( fp, lvl+1, t->u.op.o1 );
		dump_tree( fp, lvl+1, t->u.op.o2 );
		dump_tree( fp, lvl+1, t->u.op.o3 );
		break;
	default:
		qp_abort( "dump_op_node: Unkown OpType %d\n", t->u.op.op );
		break;
	}
}



static void
dump_name_node( FILE* const fp, int lvl, const pTree t )
{
	int	i;

	spaces( fp, lvl );
	fprintf( fp, "%s", t->u.name.name );


	if ( ! dim_static( t->dim ) ) {
		fputs( " < ", fp );
		dump_dimension( fp, t->u.name.sig->dim );
		fputs( " >", fp );
	}

	fputc( '\n', fp );

	if ( t->u.name.argc > 0 ) {
		spaces( fp, lvl );
		fputs( "(\n", fp );
	}

	for( i=0; i < t->u.name.argc ; i++ ) {
		dump_tree( fp, lvl+1, t->u.name.argv[i] );
	}

	if ( t->u.name.argc > 0 ) {
		spaces( fp, lvl );
		fputs( ")\n", fp );
	}
	
	if ( t->u.name.range ) {
		spaces( fp, lvl );
		fputs( "(\n", fp );
		dump_tree( fp, lvl+1, t->u.name.range );
		spaces( fp, lvl );
		fputs( ")\n", fp );
	}
}


static void
dump_const_node( FILE* const fp, int lvl, const pTree t )
{
	fputc( '\n', fp );	/* the value has been printed already ! */
}


void
dump_tree( FILE * const fp, int lvl, const pTree t )
{

	spaces( fp, lvl );
	fprintf( fp, "Node: %-8s ", _nodetype_to_str[t->ntyp] );
	if ( t->dim != 0 ) {
		dump_dimension( fp, t->dim );
	}
	if ( t->val != 0 ) {
		val_dump( fp, lvl, t->val );
	}
	if ( t->check_shape > 0 ) 
		fputs( " check_shape", fp );

	if ( t->need_shape ) 
		fputs( " need_shape", fp );

	fputc( '\n', fp );

	switch( t->ntyp ) {	/* switch NodeType */
	case N_CUT:		/* predefined cut */
			spaces( fp, lvl );
			fprintf( fp, "$%d\n", t->u.cut.cid );
			break;

	case N_CONST:	/* leaf, constant */
			dump_const_node( fp, lvl, t );
			break;

	case N_OP:		/* operator (unary or binary) */
			dump_op_node( fp, lvl, t );
			break;

	case N_NAME:	/* function or array or string with range, sorted out later */
			dump_name_node( fp, lvl, t );
			break;

	case N_RANGE:	/* range */
			if ( t->u.range.from ) {
				dump_tree( fp, lvl+1, t->u.range.from );
			}
			spaces( fp, lvl+1 );
			fputs( ":\n", fp );
			if ( t->u.range.to ) {
				dump_tree( fp, lvl+1, t->u.range.to );
			}
			break;

	case N_MASK:	/* mask writing */
			spaces( fp, lvl+1 );
			fprintf( fp, "Mask \"%s\" Index:\n", t->u.mask.name );
			dump_tree( fp, lvl+2, t->u.mask.index );
			spaces( fp, lvl+1 );
			fputs( "Value:\n", fp );
			dump_tree( fp, lvl+2, t->u.mask.o1 );
			break;

	default:
			qp_abort( "dump_tree: Unkown NodeType %d\n", t->ntyp );
			break;
	}

}
