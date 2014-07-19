/*
 *  qp_exe_op_str.h  --
 *
 *  Original: 23-Jan-1996 12:09
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_op_str.h,v 1.4 1996/04/23 18:38:33 maartenb Exp $
 *
 *  $Log: qp_exe_op_str.h,v $
 *  Revision 1.4  1996/04/23 18:38:33  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_LT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) < 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) <= 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LTLT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) < 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) < 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LTLE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) < 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) <= 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LELT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) <= 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) < 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_LELE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) <= 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) <= 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) > 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) >= 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;


case FC_GTGT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) > 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) > 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GTGE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) > 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) >= 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GEGT + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) >= 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) > 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_GEGE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	o3 = &stack[frame[framep-2]];
	POP_FRAME(3);
	NEW_FRAME(D_BOOL,1,r);
	if ( (strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) >= 0) && (strncasecmp( (char *) o2, (char *) o3, QP_STR_MAX ) >= 0) ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;


case FC_EQ + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) == 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

case FC_NE + (D_STR * 64):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) != 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;

/* this needs a specific implementation, probably ... :-( */
case FC_CT + (D_STR * 64):
	sf_report( "Warning: FC_CT not implemented properly\n" );
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(2);
	NEW_FRAME(D_BOOL,1,r);
	if ( strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX ) == 0 ) {
		*((bool *) r) = TRUE;
	} else {
		*((bool *) r) = FALSE;
	}
	break;
