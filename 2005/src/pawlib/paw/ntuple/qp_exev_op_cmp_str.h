/*
 *  qp_exev_op_cmp.h  --
 *
 *  Original: 23-Jan-1995 17:17
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exev_op_cmp_str.h,v 1.4 1996/04/23 18:38:40 maartenb Exp $
 *
 *  $Log: qp_exev_op_cmp_str.h,v $
 *  Revision 1.4  1996/04/23 18:38:40  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_LT + (D_STR * 64):
case FC_LE + (D_STR * 64):
case FC_GT + (D_STR * 64):
case FC_GE + (D_STR * 64):
case FC_EQ + (D_STR * 64):
case FC_NE + (D_STR * 64):
case FC_CT + (D_STR * 64):
{
	char		tmp1[QP_STR_MAX], tmp2[QP_STR_MAX];
	int		i, n = 1, d;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[D_STR];
		n = frame_size[framep];
	} else {
		strncpy( tmp1, (char *) &stack[frame[framep]], QP_STR_MAX );
		o1 = (Int32 *) tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[D_STR];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		strncpy( tmp2, (char *) &stack[frame[framep-1]], QP_STR_MAX );
		o2 = (Int32 *) tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {

		*((bool *) r) = FALSE;
		d = strncasecmp( (char *) o1, (char *) o2, QP_STR_MAX );
		switch ( fc ) {
		case FC_LT+(D_STR * 64): if ( d < 0 ) *((bool*)r)=TRUE;break;
		case FC_LE+(D_STR * 64): if ( d <= 0 ) *((bool*)r)=TRUE;break;
		case FC_GT+(D_STR * 64): if ( d > 0 ) *((bool*)r)=TRUE;break;
		case FC_GE+(D_STR * 64): if ( d >= 0 ) *((bool*)r)=TRUE;break;
		case FC_EQ+(D_STR * 64): if ( d == 0 ) *((bool*)r)=TRUE;break;
		case FC_NE+(D_STR * 64): if ( d != 0 ) *((bool*)r)=TRUE;break;
		case FC_CT+(D_STR * 64): if ( d == 0 ) *((bool*)r)=TRUE;break;
		}

		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}



/*
 *  Combine these entries to shrink the case statement, to avoid 
 *  HP-C case statement bug :-(
 */
case FC_LTLT + (D_STR * 64):
case FC_LTLE + (D_STR * 64):
case FC_LELT + (D_STR * 64):
case FC_LELE + (D_STR * 64):
case FC_GTGT + (D_STR * 64):
case FC_GTGE + (D_STR * 64):
case FC_GEGT + (D_STR * 64):
case FC_GEGE + (D_STR * 64):
{
	char		tmp1[QP_STR_MAX], tmp2[QP_STR_MAX], tmp3[QP_STR_MAX];
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[D_STR];
		n = frame_size[framep];
	} else {
		strncpy( tmp1, (char *) &stack[frame[framep]], QP_STR_MAX );
		o1 = (Int32 *) tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[D_STR];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		strncpy( tmp2, (char *) &stack[frame[framep-1]], QP_STR_MAX );
		o2 = (Int32 *) tmp2;
		inc_o2 = 0;
	}
	if ( frame_size[framep-2] != 1 ) {
		o3 = &stack[frame[framep-2]];
		inc_o3 = datatype_size[D_STR];
		qp_assert( n==1 || n==frame_size[framep-2] );
		n = frame_size[framep-2];
	} else {
		strncpy( tmp3, (char *) &stack[frame[framep-2]], QP_STR_MAX );
		o3 = (Int32 *) tmp3;
		inc_o3 = 0;
	}
	POP_FRAME(3);

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	o3 += inc_o3 * (n-1);
	r += inc_r * (n-1);

	switch ( fc ) {
	case FC_LTLT + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) < 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) < 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LTLE + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) < 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) <= 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LELT + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) <= 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) < 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LELE + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) <= 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) <= 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GTGT + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) > 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) > 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GTGE + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) > 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) >= 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GEGT + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) >= 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) > 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GEGE + (D_STR * 64):
		for ( i=0 ; i < n ; i++ ) {
			if ( (strncasecmp( (char *)o1, (char *)o2, QP_STR_MAX) >= 0) &&
				(strncasecmp( (char *)o2, (char *)o3, QP_STR_MAX) >= 0)
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	default:
		qp_assert(0);
		break;
	}
	break;
}



