/*
 *  qp_cuts.h  --
 *	define the structure for the cut store and
 *	declare the routines to access it.
 *
 *  Original: 31-Jan-1995 15:21
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_cuts.h,v 1.5 1996/04/23 18:38:14 maartenb Exp $
 *
 *  $Log: qp_cuts.h,v $
 *  Revision 1.5  1996/04/23 18:38:14  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_CUTS
#define CERN_QP_CUTS


#define MAX_CUT_ID	99


typedef enum {
	C_NONE,
	C_EXPR,
	C_GRAF
} CutType;


typedef	float	(*CutData)[2];


typedef struct _cut_expr_ {
	char	*expr;
} CutExpr;


typedef struct _cut_graf_ {
	int	dim;
	char	*exprx;
	char	*expry;
	int	np;
	CutData	points;
} CutGraf;


typedef struct _cut_ {
	int	id;
	CutType		ctyp;	/* C_EXPR or C_GRAF */

	/* the dependency info */

	int	size;
	int	num;
	short	*dep;

	/* specific info */

	union {
		CutExpr	expr;
		CutGraf	graf;
	} u;

} Cut;


CutType
qp_cut_type( int cid );

char *
qp_cut_expr( int cid );

void
qp_gcut_expr( int cid, char **x, char **y );

void
qp_cut_add_dep( int cid, int used_cid, int * errp );

int
cut_get_cid( char * const cid_str );

void
gcut_add_1d( int cid, const char * const expr, float xlow, float xhigh );

void
gcut_add_2d( int cid, const char * const expr_x, const char * const expr_y,
	int n, float xv[], float yv[] );

CutGraf *
gcut_get_data( const int cid );

#endif	/*	CERN_QP_CUTS	*/
