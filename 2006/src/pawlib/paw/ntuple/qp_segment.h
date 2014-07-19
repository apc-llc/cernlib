/*
 *  qp_segment.h  --
 *
 *  Original: 11-Jan-1995 15:33
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_segment.h,v 1.3 1996/04/23 18:38:58 maartenb Exp $
 *
 *  $Log: qp_segment.h,v $
 *  Revision 1.3  1996/04/23 18:38:58  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_SEGMENT
#define CERN_QP_SEGMENT

#include	<stdio.h>


#include	"cern_types.h"

typedef Int32 * Segment;	/* forward declaration */

#include	"qp_query.h"



typedef struct _label_ {

	Segment		addr;	/* the address this label points to */

	int		n;
	int		size;
	Segment		*ref;	/* list of references to be filled in */

} Label;


typedef struct _label_tab_ {
	int		n;
	int		size;
	Label		*tab;	/* list of labels */
} LabelTab;


typedef struct _segbuild_ {

	/* the assembly segment */
	bool		ok;
	int		n;
	Segment		seg;
	Segment		cur;
	Segment		last_opc;

	/* label table */
	LabelTab	*lt;

} SegBuild;


SegBuild *
new_segbuild( int * errp );

void
free_segbuild( SegBuild * sbp );

Segment
segment_from_segbuild( SegBuild *sbp );

int
qp_seg_new_label( SegBuild * sbp, int *errp );

bool
qp_seg_flag_opc( SegBuild * sbp, Int32 v, int * errp );

bool
qp_seg_emit_opc( SegBuild * sbp, Int32 v, int * errp );

bool
qp_seg_emit_word( SegBuild * sbp, Int32 v, int * errp );

bool
qp_seg_emit_ref( SegBuild * sbp, int lid, int *errp );

bool
qp_seg_emit_label( SegBuild * sbp, int lid, int *errp );

bool
qp_seg_labels_resolved( SegBuild * sbp, int *errp );


void
dump_segment( FILE * fp, char * leader, QueryExe * qe, Segment s );

#endif	/*	CERN_QP_SEGMENT	*/
