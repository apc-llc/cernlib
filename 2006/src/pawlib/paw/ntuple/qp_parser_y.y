/*
 *  parser.y  -  yacc parser for selfun
 *
 *  Original: 15-Jun-1994 15:20
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_parser_y.y,v 1.10 1996/05/14 12:23:29 maartenb Exp $
 *
 *  $Log: qp_parser_y.y,v $
 *  Revision 1.10  1996/05/14 12:23:29  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.9  1996/04/23 18:38:51  maartenb
 *  - Add RCS keywords
 *
 *
 */

%{

#include	<string.h>

#include	"qp_lexyacc.h"
#include	"mstr.h"
#include	"qp_tree.h"
#include	"qp_report.h"
#include	"str.h"

pTree	qp_the_parse_tree;	/* return the parse tree */

%}

%union {
/* Derived from $Id: qp_parser_y.y,v 1.10 1996/05/14 12:23:29 maartenb Exp $ */
	int	num;
	double	real;
	char	* str;
	pTree	expr;
	pTree	expr_list;
#define yylval	qp_parser_lval
}

%token		T_UNKNOWN
%token	<num>	T_CUT
%token	<str>	T_IDENT
%token	<num>	T_BOOL
%token	<num>	T_UNSIGNED
%token	<num>	T_INT
%token	<real>	T_FLOAT
%token	<real>	T_DOUBLE
%token	<str>	T_STRING
%token		T_PUT_MASK
%token		T_IN

%left		T_OR
%left		T_AND
%right		T_NOT
%nonassoc <num>	T_EQ T_NE T_CT T_LT T_LE T_GT T_GE
%left		'+' '-'
%left		'*' '/'
%left		UMINUS
%right		T_POWER

%type	<num>	lte_op gte_op

%type	<expr>	bool_expr bool_term bool_fact
%type	<expr>	comparison
%type	<expr>	expression function_array argument
%type	<expr>	range

%type	<expr_list>	arg_list one_or_more_args

%type	<str>	name simple_file
/*
%type	<str>	unix_file unix_path
%type	<str>	vms_file vms_dir vms_path vmcms_file vms_disk
*/

%%

line		:
			bool_expr
			{
				qp_the_parse_tree =  $1;
			}
		|
			bool_expr T_PUT_MASK T_IDENT "(" expression ")"
			{
				qp_the_parse_tree = new_mask_node( $1, $3, $5 );
			}
		;


bool_expr:
			bool_term
		|	bool_expr T_OR bool_term
			{ $$ = new_op_node( O_OR, $1, $3, 0 ); }
		;

bool_term:
			bool_fact
		|	bool_term T_AND bool_fact
			{ $$ = new_op_node( O_AND, $1, $3, 0 ); }
		;

bool_fact:
			comparison
		|	T_NOT bool_fact
			{ $$ = new_op_node( O_NOT, $2, 0, 0 ); }
		;

lte_op		:
			T_LT
			{ $$ = O_LT; }
		|	T_LE
			{ $$ = O_LE; }
		;
			
gte_op		:
			T_GT
			{ $$ = O_GT; }
		|	T_GE
			{ $$ = O_GE; }
		;
			
comparison	:
			expression
		|	expression lte_op expression
			{ $$ = new_op_node( $2 , $1, $3, 0 ); }
		|	expression lte_op expression lte_op expression
			{
				pTree	c1, c2;
				OpType	op;

				if ( $2 == O_LT ) {
					if ( $4 == O_LT ) 	op = O_LTLT;
					else 			op = O_LTLE;
				} else {
					if ( $4 == O_LT ) 	op = O_LELT;
					else 			op = O_LELE;
				}


				$$ = new_op_node( op, $1, $3, $5 );
			}
		|	expression gte_op expression
			{ $$ = new_op_node( $2, $1, $3, 0 ); }
		|	expression gte_op expression gte_op expression
			{
				pTree	c1, c2;
				OpType	op;

				if ( $2 == O_GT ) {
					if ( $4 == O_GT ) 	op = O_GTGT;
					else 			op = O_GTGE;
				} else {
					if ( $4 == O_GT ) 	op = O_GEGT;
					else 			op = O_GEGE;
				}


				$$ = new_op_node( op, $1, $3, $5 );
			}
		|	expression T_EQ expression
			{ $$ = new_op_node( O_EQ, $1, $3, 0 ); }
		|	expression T_NE expression
			{ $$ = new_op_node( O_NE, $1, $3, 0 ); }
		|	expression T_CT expression
			{ $$ = new_op_node( O_CT, $1, $3, 0 ); }
		;

expression	:
			T_BOOL
			{ $$ = new_con_bool_node( $1 ); }
		|	T_UNSIGNED
			{ $$ = new_con_uint_node( $1 ); }
		|	T_INT
			{ $$ = new_con_int_node( $1 ); }
		|	T_FLOAT
			{ $$ = new_con_float_node( $1 ); }
		|	T_DOUBLE
			{ $$ = new_con_double_node( $1 ); }
		|	T_STRING
			{
				$$ = new_con_str_node( $1 );
				mstr_del( $1 );
			}
		|	T_CUT
			{ $$ = new_cut_node( $1 ); }
		|	function_array

		|	expression '+' expression
			{ $$ = new_op_node( O_PLUS, $1, $3, 0 ); }
		|	expression '-' expression
			{ $$ = new_op_node( O_MINUS, $1, $3, 0 ); }
		|	expression '*' expression
			{ $$ = new_op_node( O_TIMES, $1, $3, 0 ); }
		|	expression '/' expression
			{ $$ = new_op_node( O_DIV, $1, $3, 0 ); }
		|	'-' expression %prec UMINUS
			{ $$ = new_op_node( O_UMINUS, $2, 0, 0 ); }
		|	'+' expression %prec UMINUS
			{ $$ = $2; }
		|	expression T_POWER expression
			{ $$ = new_op_node( O_POW, $1, $3, 0 ); }
		|	'(' bool_expr ')'
			{ $$ = $2; }
		;

function_array	:
			name	/* I am not so happy about this */
			{ 
				mstr_release( $1 );
				$$ = new_name_node( $1, 0, 0 );
			}
		|	name "(" arg_list ")"
			{
				mstr_release( $1 );
				$$ = new_name_node( $1, $3, 0 );
			}
		|	name "(" arg_list ")" "(" argument ")"
			{
				mstr_release( $1 );
				$$ = new_name_node( $1, $3, $6 );
			}
		;

arg_list
		:	/* empty */
			{ $$ = 0; }
		|	one_or_more_args
		;

one_or_more_args
		:	argument
			{ $1->next = 0; $$ = $1; }
		|	one_or_more_args "," argument
			{ $3->next = $1; $$ = $3; }
		;

argument
		:	range
		|	bool_expr
		;
			

name
		:	T_IDENT
		|	simple_file
/*
		|	unix_file
		|	vms_file
		|	vmcms_file
*/
		;

simple_file
		:	T_IDENT "." T_IDENT
			{ $$ = mstr_merge( $1, mstr_new("."), $3, (char*) 0 ); }
		;

/*
unix_file
		:	unix_path "/" simple_file
			{ $$ = mstr_merge( $1, mstr_new("/"), $3, (char*) 0 ); }
		|	"~" unix_path "/" simple_file
			{ $$ = mstr_merge( mstr_new("~"), $2, mstr_new("/"), $4,
					(char*) 0 ); }
		|	"~" T_IDENT unix_path "/" simple_file
			{ $$ = mstr_merge( mstr_new("~"), $2, $3, mstr_new("/"),
					$5, (char*) 0 ); }
		;

unix_path
		:	/* empty */ /*
			{ $$ = mstr_new(""); }
		|	unix_path "/"
			{ $$ = mstr_merge($1, mstr_new("/"), (char*) 0 ); }
		|	unix_path "/" T_IDENT 
			{ $$ = mstr_merge( $1, mstr_new("/"), $3, (char*) 0 ); }
		;
*/

/*
vms_file
		:	vms_dir simple_file
			{ $$ = mstr_merge( $1, $2, (char*) 0 ); }

vms_dir
		:	vms_disk "[" vms_path "]"
			{ $$ = mstr_merge( $1, mstr_new(":["), $3, mstr_new("]"),
					(char*) 0 ); }
		|	"[" vms_path "]"
			{ $$ = mstr_merge( mstr_new("["), $2, mstr_new("]"),
					(char*) 0 ); }
		;

vms_disk
		:	T_IDENT ":"
			{ $$ = $1; }
		;

vms_path
		:	T_IDENT
			{ $$ = $1; }
		|	"." T_IDENT
			{ $$ = mstr_merge( mstr_new("."), $2, (char*) 0 );}
		|	vms_path "." T_IDENT
			{ $$ = mstr_merge($1, mstr_new("."), $3, (char*) 0);}
		;
*/

/*
vmcms_file
		:	simple_file "." T_IDENT
			{ $$ = mstr_merge( $1, mstr_new("."), $3, (char*) 0);}
		;
*/

range
		:	expression ":" expression
			{ $$ = new_range_node( $1, $3 ); }
		|	expression ":"
			{ $$ = new_range_node( $1, (pTree) 0 ); }
		|	":" expression
			{ $$ = new_range_node( (pTree) 0, $2 ); }
		|	":"
			{ $$ = new_range_node( (pTree) 0, (pTree) 0 ); }
/*
		|	vms_disk
			{
				mstr_release( $1 );
				$$ = new_range_node(
					new_name_node( $1, (pTree) 0, (pTree) 0 ), (pTree) 0 );
			}
		|	vms_disk expression
			{
				mstr_release( $1 );
				$$ = new_range_node(
					new_name_node( $1, (pTree) 0, (pTree) 0 ), $2 );
			}
*/
		;
%%

int
qp_scanner_wrap( void )
{
	return 1;
}

void
qp_parser_error( char *message )
{
	char	*s;
	int	index;

	sf_report( " %s\n", message );
	qp_scanner_get_pos( &s, &index );
	if ( index != -1) {
		sf_report( " %s\n", s );
		s = str_new( s );
		memset( s, ' ', strlen( s ) );
		s[index>0 ? index-1 : 0] = '^';
		sf_report( " %s\n", s );
		str_del( s );
	}
}
