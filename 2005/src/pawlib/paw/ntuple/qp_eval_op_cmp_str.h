/*
 *  qp_eval_op_cmp_str.h  --
 *
 *  Original: 28-Mar-1996 15:17
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_op_cmp_str.h,v 1.3 1996/04/23 18:38:22 maartenb Exp $
 *
 *  $Log: qp_eval_op_cmp_str.h,v $
 *  Revision 1.3  1996/04/23 18:38:22  maartenb
 *  - Add RCS keywords
 *
 *
 */

	case FC_LT:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) < 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_LE:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) <= 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_LTLT:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) < 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) < 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_LTLE:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) < 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) <= 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_LELT:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) <= 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) < 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_LELE:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) <= 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) <= 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_GTGT:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) > 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) > 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_GTGE:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) > 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) >= 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_GEGT:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) >= 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) > 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_GEGE:
		if (
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) >= 0
			&&
			strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) >= 0
		) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;


	case FC_GT:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) > 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_GE:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) >= 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;


	case FC_EQ:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) == 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_NE:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) != 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;

	case FC_CT:
		if ( strncasecmp( valv[0]->u.OP_FIELD, valv[1]->u.OP_FIELD, QP_STR_MAX ) == 0 ) {
			r->u.bval = TRUE;
		} else {
			r->u.bval = FALSE;
		}
		r->d = dim_new( D_BOOL );
		break;
