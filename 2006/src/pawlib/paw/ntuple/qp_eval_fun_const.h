/*
 *  template_fun_0arg.c  --
 *	Evaluation of argumentless functions a.k.a builtin constants
 *
 *  Original: 25-Nov-1994 16:48
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_fun_const.h,v 1.3 1996/04/23 18:38:19 maartenb Exp $
 *
 *  $Log: qp_eval_fun_const.h,v $
 *  Revision 1.3  1996/04/23 18:38:19  maartenb
 *  - Add RCS keywords
 *
 *
 */

/* constants */
	case FC_TRUE:
		r->u.bval = TRUE;
		r->d = dim_new( D_BOOL );
		break;
	case FC_FALSE:
		r->u.bval = FALSE;
		r->d = dim_new( D_BOOL );
		break;
	case FC_PI:
		r->u.fval = M_PI;
		r->d = dim_new( D_FLOAT );
		break;
	case FC_DPI:
		r->u.lfval = M_PI;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_UINT32_MIN:
		r->u.uval = 0;
		r->d = dim_new( D_UINT );
		break;

	case FC_UINT32_MAX:
		r->u.uval = UINT32_MAX;
		r->d = dim_new( D_UINT );
		break;

	case FC_UINT64_MIN:
		r->u.luval = 0;
		r->d = dim_new( D_ULONG );
		break;

	case FC_UINT64_MAX:
		r->u.luval = UINT64_MAX;
		r->d = dim_new( D_ULONG );
		break;


	case FC_INT32_MIN:
		r->u.ival = INT32_MIN;
		r->d = dim_new( D_INT );
		break;

	case FC_INT32_MAX:
		r->u.ival = INT32_MAX;
		r->d = dim_new( D_INT );
		break;

	case FC_INT64_MIN:
		r->u.lival = INT64_MIN;
		r->d = dim_new( D_LONG );
		break;

	case FC_INT64_MAX:
		r->u.lival = INT64_MAX;
		r->d = dim_new( D_LONG );
		break;


	case FC_FLOAT32_MIN:
		r->u.fval = FLOAT32_MIN;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FLOAT32_MAX:
		r->u.fval = FLOAT32_MAX;
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FLOAT64_MIN:
		r->u.lfval = FLOAT64_MIN;
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_FLOAT64_MAX:
		r->u.lfval = FLOAT64_MAX;
		r->d = dim_new( D_DOUBLE );
		break;
