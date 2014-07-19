/*
 *  qp_tree.h  --
 *
 *  Original: 10-Oct-1994 13:53
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_tree.h,v 1.8 1996/04/23 18:39:06 maartenb Exp $
 *
 *  $Log: qp_tree.h,v $
 *  Revision 1.8  1996/04/23 18:39:06  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_TREE
#define CERN_TREE

#include	<stdio.h>


typedef struct _tree_ *pTree;

#include	"cern_types.h"
#include	"qp_dimension.h"
#include	"qp_value.h"
#include	"qp_convert.h"
#include	"qp_signature.h"
#include	"qp_cuts.h"


extern bool	tag_alloc_flag;

extern pTree	qp_the_parse_tree;

typedef enum _op_type_ {
	O_OR,
	O_AND,
	O_NOT,

	O_LT,
	O_LE,

	O_LTLT,
	O_LTLE,
	O_LELT,
	O_LELE,

	O_GT,
	O_GE,

	O_GTGT,
	O_GTGE,
	O_GEGT,
	O_GEGE,

	O_EQ,
	O_NE,
	O_CT,

	O_PLUS,
	O_MINUS,
	O_TIMES,
	O_DIV,

	O_UMINUS,

	O_POW,

	O_VADD,
	O_VMUL,
	O_VAND,
	O_VOR,
	O_VMIN,
	O_VMAX,

	O_MIN,
	O_MAX

} OpType;


typedef struct _node_cut_ {
	int		cid;		/* the cut id ( n in $n ) 	      */
	CutType		ctyp;		/* the type of the cut 		      */
	int		index;		/* the index of the cut in this query */
} NodeCut;


typedef struct _node_op_ {
	OpType		op;		/* the operator type */
	pTree		o1, o2, o3;	/* the operants from left to right */
} NodeOp;


typedef struct _node_name_ {
	char		*name;
	STIndex		idx;
	Signature	*sig;
	int		argc;
	pTree		argv[MAX_DIMENSION];

	/* possible subrange specifier for string types */
	pTree		range;

	bool		check[MAX_DIMENSION];	/* args which need to be */
						/* checked with given dim */
						/* (static formal array arg */
						/* with dynamic actual arg ) */
	
} NodeName;


typedef struct _node_range_ {
	pTree	from, to;
} NodeRange;


typedef struct _node_mask_ {
	pTree	o1;
	char	*name;	/* name of the mask to be written */
	STIndex	idx;
	pTree	index;	/* expression of the mask bit to be written */
} NodeMask;


typedef enum _node_type_	{
	N_CUT = 1,	/* predefined cut */
	N_CONST,	/* leaf, constant */
	N_OP,		/* operator (unary or binary) */
	N_NAME,		/* identifier with 0 or more args, optional subrange*/
	N_RANGE,	/* range for array indexing */
	N_MASK		/* describe output to mask */
} NodeType;


#if 0
typedef struct _range_ {
	int	from, to;
} Range;
#endif


typedef struct _tree_ {

	/* start of construct in the expression string */
	int		index;

	/* list of nodes allocated during this parse operation */
	pTree		next_alloc;

	/* possibility to make lists op expr's */
	pTree		next;

	/* type of this node ... */
	NodeType	ntyp;

	/* node type dependent fields */ union {
		NodeCut		cut;
		NodeOp		op;
		NodeName	name;
		NodeRange	range;
		NodeMask	mask;
	} u;

	/* semantic checking done for this subtree */
	bool		sem_ok;

	/* Dynamic type info */
	Dimension	*dim;
	int		check_shape; /* number of shapes to be matched*/
	bool		need_shape; /* leave one shape on shape stack */

	/* value(s) associated with this node */
	Value		*val;
} Tree;


/* function prototypes */

void
free_tree( const pTree t );

void
free_node( const pTree t );


void
print_tree( FILE * const fp, const pTree t );


void
dump_tree( FILE * const fp, int lvl, const pTree t );


/*
 *  Create operator nodes
 */

pTree
new_op_node( const OpType op, const pTree o1, const pTree o2, const pTree o3 );


/*
 *  Create litteral nodes
 */

pTree
new_con_bool_node( const bool bval );

pTree
new_con_uint_node( const unsigned int uval );

pTree
new_con_int_node( const int ival );

pTree
new_con_float_node( const double fval );

pTree
new_con_double_node( const double lfval );

pTree
new_con_str_node( char *const sval );


/*
 *  Create cut nodes
 */

pTree
new_cut_node( const int num );


/*
 *  Create name (identifier) nodes
 */

pTree
new_name_node( char *const name, const pTree arg_list, const pTree sub_range );


/*
 *  Create range node
 */

pTree
new_range_node( const pTree from, const pTree to );


/*
 *  Create type conversion node
 */

pTree
new_conv_node( const pTree t, const ConvType c );


/*
 *  Create mask writing node
 */

pTree
new_mask_node( const pTree t, char * const name, const pTree index );


/*
 *  Track allocations during parsing
 */

void
reset_alloc_list( void );

void
clear_alloc_list( void );

void
free_alloc_list( void );


/*
 *  Readable names for constants
 */

char *
convtype_to_str( const ConvType c );

char *
nodetype_to_str( const NodeType n );

char *
optype_to_str( const OpType o );

FCode
optype_to_fc( const OpType o );

OpType
fc_to_optype( const FCode fc );

#endif	/*	CERN_TREE	*/
