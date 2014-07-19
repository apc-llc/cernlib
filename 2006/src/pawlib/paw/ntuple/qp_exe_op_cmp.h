/*
 *  qp_exe_op_cmp.h  --
 *
 *  Original: 23-Jan-1995 17:17
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_op_cmp.h,v 1.3 1996/04/23 18:38:32 maartenb Exp $
 *
 *  $Log: qp_exe_op_cmp.h,v $
 *  Revision 1.3  1996/04/23 18:38:32  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_LT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1)) < (*((OP_TYPE *) o2)) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) <= *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LTLT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) < *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) < *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LTLE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) < *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) <= *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LELT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) <= *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) < *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LELE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) <= *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) <= *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) > *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) >= *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;


case FC_GTGT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) > *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) > *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GTGE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) > *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) >= *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GEGT + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) >= *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) > *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GEGE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (*((OP_TYPE *) o1) >= *((OP_TYPE *) o2)) &&
		(*((OP_TYPE *) o2) >= *((OP_TYPE *) o3))
	) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;


case FC_EQ + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) == *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_NE + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) != *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

#if 0
/* this needs a specific implementation, probably ... :-( */
case FC_CT + (OP_BASE):
	sf_report( "Warning: FC_CT not implemented properly\n" );
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( *((OP_TYPE *) o1) == *((OP_TYPE *) o2) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;
#endif
