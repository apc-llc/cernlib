/*
 *  qp_query.h  --
 *
 *  Original: 24-Nov-1994 10:47
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_query.h,v 1.14 1999/08/31 08:47:38 couet Exp $
 *
 *  $Log: qp_query.h,v $
 *  Revision 1.14  1999/08/31 08:47:38  couet
 *  - A new set of PAW commands using Neural Networks.
 *    These new commands are using the MLPfit package.
 *
 *  Revision 1.13  1996/05/12 15:13:54  maartenb
 *  - Replaced the handling of variables. The handling of variables is now
 *  fully dynamic and there is no other limit than the commonblocks to the
 *  number of var's in a query.
 *
 *  Revision 1.12  1996/04/23 18:38:54  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_QUERY
#define CERN_QP_QUERY

/* forward declaration */
typedef struct _query_exe_ QueryExe;


#include	"cern_types.h"
#include	"hbook_defs.h"
#include	"qp_name.h"
#include	"qp_segment.h"
#include	"qp_tree.h"
#include	"qp_mask.h"
#include	"qp_vvec.h"
#include	"svec.h"


#define	MAX_EXPRS	30
#define MAX_CUTS	50
#define MAX_MASKS	5


typedef struct _query_src_ QuerySrc;
struct _query_src_ {

	char *		path;		/* the ntuple path */
	int		id;		/* the ntuple id */

	bool		hasSel;
	String		sel;

	int		nexpr;
	String		expr[MAX_EXPRS];/* array of expressions for 	  */
					/* plotting/projecting/etc	  */
	int		ncuts;
	int		cuts[MAX_CUTS];/* array of cuts used in sel,expr */
					/* and other cuts. filled during  */
					/* parsing !! */
};

typedef struct _query_tree_ QueryTree;
struct _query_tree_ {

	char *		path;		/* the ntuple path */
	int		id;		/* the ntuple id */

	int		check_shape;	/* number of frames to check at top level */

	bool		hasSel;
	pTree		sel;

	int		nexpr;
	pTree		expr[MAX_EXPRS];/* array of parse trees for 	  */
					/* plotting/projecting/etc	  */

	int		ncuts;
	int		cuts_cid[MAX_CUTS]; /* The cut id for this cut */
	pTree		cuts[MAX_CUTS];	/* array of parse trees for	  */
					/* the cuts used in the selection */
					/* and the expressions. The index */
					/* is a local mapping not the cid */
	
	int		nmasks;
	Mask		mask[MAX_MASKS];/* the array of mask ref's used   */
					/* by the sel and exprs		  */
};


struct _query_exe_ {

	char *		path;		/* the ntuple path */
	int		id;		/* the ntuple id */
	bool		new_ntuple;

	int		check_shape;	/* number of frames to check at top level */

	bool		hasSel;
	Segment		sel;
	int		sel_size;	/* used to allocate a buffer */
					/* in # Int32			  */
	char		*sel_str;

	int		nexpr;
	Segment		expr[MAX_EXPRS];/* array of assembly segments for */
					/* plotting/projecting/etc	  */
	int		expr_size[MAX_EXPRS]; /* used to allocate a buffer */
					/* in # Int32			  */
	DataType	expr_type[MAX_EXPRS]; /* used for minmax */
	char *		expr_str[MAX_EXPRS]; /* used for axis titles */

	int		ncuts;
	Segment		cuts[MAX_CUTS];	/* array of assembly segments for */
					/* the cuts used in the selection */
					/* and the expressions. The index */
					/* is a local mapping not the $nn */
	int		cuts_size[MAX_CUTS]; /* used to allocate a buffer */
					/* in # Int32			  */
	DataType	cuts_type[MAX_CUTS]; /* used to alloc stack frame */
	bool		cuts_has_shape[MAX_CUTS]; /* does cut leave shape */
						/* info on the stack ? */
	int		cuts_cid[MAX_CUTS]; /* The cut id for this cut */
	
	VVec		vv;		/* the ntuple columns needed for  */
					/* this query			  */
	int		total_size;	/* maximum storage for all variables */

	int		nmasks;
	Mask		*mask[MAX_MASKS];/* the array of mask ref's used   */
					/* by the sel and exprs		  */
	int		mask_write;	/* The index of the mask that is writable */
	int		mask_ibit;	/* the bit that is written */
};


QuerySrc *
qp_qs_new(
	char *		path,
	int		id,
	String		select,
	SVec		expressions
);

void
qp_qs_free( QuerySrc * qs );

QueryTree *
qp_qt_new( char * caller, int * errp );

void
qp_qt_free( QueryTree * qt );

QueryExe *
qp_qe_new( char * caller, int * errp );

void
qp_qe_free( QueryExe * qe );

bool
qp_has_string_expr( QueryExe * qe );

#endif	/*	CERN_QP_QUERY	*/
