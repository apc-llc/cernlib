/*
 *  qp_eval_conv.h  --
 *
 *  Original: 23-Apr-1996 20:25
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_conv.h,v 1.3 1996/04/23 18:38:17 maartenb Exp $
 *
 *  $Log: qp_eval_conv.h,v $
 *  Revision 1.3  1996/04/23 18:38:17  maartenb
 *  - Add RCS keywords
 *
 *
 */

	case FC_B_2_B:
		r->u.bval = (bool) valv[0]->u.bval ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_B_2_U:
		r->u.uval = (UInt32) valv[0]->u.bval;
		r->d = dim_new( D_UINT );
		break;

	case FC_B_2_LU:
		r->u.luval = (UInt64) valv[0]->u.bval;
		r->d = dim_new( D_ULONG );
		break;

	case FC_B_2_I:
		r->u.ival = (Int32) valv[0]->u.bval;
		r->d = dim_new( D_INT );
		break;

	case FC_B_2_LI:
		r->u.lival = (Int64) valv[0]->u.bval;
		r->d = dim_new( D_LONG );
		break;

	case FC_B_2_F:
		r->u.fval = (Float32) valv[0]->u.bval;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_B_2_LF:
		r->u.lfval = (Float64) valv[0]->u.bval;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_U_2_B:
		r->u.bval = (bool) valv[0]->u.uval ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_U_2_U:
		r->u.uval = (UInt32) valv[0]->u.uval;
		r->d = dim_new( D_UINT );
		break;

	case FC_U_2_LU:
		r->u.luval = (UInt64) valv[0]->u.uval;
		r->d = dim_new( D_ULONG );
		break;

	case FC_U_2_I:
		r->u.ival = (Int32) valv[0]->u.uval;
		r->d = dim_new( D_INT );
		break;

	case FC_U_2_LI:
		r->u.lival = (Int64) valv[0]->u.uval;
		r->d = dim_new( D_LONG );
		break;

	case FC_U_2_F:
		r->u.fval = (Float32) valv[0]->u.uval;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_U_2_LF:
		r->u.lfval = (Float64) valv[0]->u.uval;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_LU_2_B:
		r->u.bval = (bool) valv[0]->u.luval ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LU_2_U:
		r->u.uval = (UInt32) valv[0]->u.luval;
		r->d = dim_new( D_UINT );
		break;

	case FC_LU_2_LU:
		r->u.luval = (UInt64) valv[0]->u.luval;
		r->d = dim_new( D_ULONG );
		break;

	case FC_LU_2_I:
		r->u.ival = (Int32) valv[0]->u.luval;
		r->d = dim_new( D_INT );
		break;

	case FC_LU_2_LI:
		r->u.lival = (Int64) valv[0]->u.luval;
		r->d = dim_new( D_LONG );
		break;

	case FC_LU_2_F:
		r->u.fval = (Float32) valv[0]->u.luval;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_LU_2_LF:
		r->u.lfval = (Float64) valv[0]->u.luval;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_I_2_B:
		r->u.bval = (bool) valv[0]->u.ival ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_I_2_U:
		r->u.uval = (UInt32) valv[0]->u.ival;
		r->d = dim_new( D_UINT );
		break;

	case FC_I_2_LU:
		r->u.luval = (UInt64) valv[0]->u.ival;
		r->d = dim_new( D_ULONG );
		break;

	case FC_I_2_I:
		r->u.ival = (Int32) valv[0]->u.ival;
		r->d = dim_new( D_INT );
		break;

	case FC_I_2_LI:
		r->u.lival = (Int64) valv[0]->u.ival;
		r->d = dim_new( D_LONG );
		break;

	case FC_I_2_F:
		r->u.fval = (Float32) valv[0]->u.ival;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_I_2_LF:
		r->u.lfval = (Float64) valv[0]->u.ival;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_LI_2_B:
		r->u.bval = (bool) valv[0]->u.lival ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LI_2_U:
		r->u.uval = (UInt32) valv[0]->u.lival;
		r->d = dim_new( D_UINT );
		break;

	case FC_LI_2_LU:
		r->u.luval = (UInt64) valv[0]->u.lival;
		r->d = dim_new( D_ULONG );
		break;

	case FC_LI_2_I:
		r->u.ival = (Int32) valv[0]->u.lival;
		r->d = dim_new( D_INT );
		break;

	case FC_LI_2_LI:
		r->u.lival = (Int64) valv[0]->u.lival;
		r->d = dim_new( D_LONG );
		break;

	case FC_LI_2_F:
		r->u.fval = (Float32) valv[0]->u.lival;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_LI_2_LF:
		r->u.lfval = (Float64) valv[0]->u.lival;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_F_2_B:
		r->u.bval = (bool) valv[0]->u.fval ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_F_2_U:
		r->u.uval = (UInt32) valv[0]->u.fval;
		r->d = dim_new( D_UINT );
		break;

	case FC_F_2_LU:
		r->u.luval = (UInt64) valv[0]->u.fval;
		r->d = dim_new( D_ULONG );
		break;

	case FC_F_2_I:
		r->u.ival = (Int32) valv[0]->u.fval;
		r->d = dim_new( D_INT );
		break;

	case FC_F_2_LI:
		r->u.lival = (Int64) valv[0]->u.fval;
		r->d = dim_new( D_LONG );
		break;

	case FC_F_2_F:
		r->u.fval = (Float32) valv[0]->u.fval;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_F_2_LF:
		r->u.lfval = (Float64) valv[0]->u.fval;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_LF_2_B:
		r->u.bval = (bool) valv[0]->u.lfval ? 1 : 0;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LF_2_U:
		r->u.uval = (UInt32) valv[0]->u.lfval;
		r->d = dim_new( D_UINT );
		break;

	case FC_LF_2_LU:
		r->u.luval = (UInt64) valv[0]->u.lfval;
		r->d = dim_new( D_ULONG );
		break;

	case FC_LF_2_I:
		r->u.ival = (Int32) valv[0]->u.lfval;
		r->d = dim_new( D_INT );
		break;

	case FC_LF_2_LI:
		r->u.lival = (Int64) valv[0]->u.lfval;
		r->d = dim_new( D_LONG );
		break;

	case FC_LF_2_F:
		r->u.fval = (Float32) valv[0]->u.lfval;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_LF_2_LF:
		r->u.lfval = (Float64) valv[0]->u.lfval;
		r->d = dim_new( D_DOUBLE );
		break;

