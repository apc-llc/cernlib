/*
 *  qp_eval_fun_single_math.h  --
 *	Implement the math functions for single precision.
 *
 *  Original:  8-Jan-1995 15:19
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_fun_single_math.h,v 1.9 1996/05/13 08:19:07 maartenb Exp $
 *
 *  $Log: qp_eval_fun_single_math.h,v $
 *  Revision 1.9  1996/05/13 08:19:07  maartenb
 *  - Removed remaining warning message about pow() operator.
 *
 *  Revision 1.8  1996/05/13 07:53:55  maartenb
 *  - Fix domain checking for log() and log10().
 *
 *  Revision 1.7  1996/04/23 18:38:20  maartenb
 *  - Add RCS keywords
 *
 *
 */
	case FC_FSIN:
		r->u.fval = sin( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FCOS:
		r->u.fval = cos( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FSQRT:
		if ( valv[0]->u.fval >= 0 ) {
			r->u.fval = sqrt( valv[0]->u.fval );
		} else {
			sf_report( "Sqrt of negative number\n" );
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FEXP:
		r->u.fval = exp( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FLOG:
		if ( valv[0]->u.fval > 0 ) {
			r->u.fval = log( valv[0]->u.fval );
		} else {
			sf_report( "Log() of non positive number\n");
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FATAN:
		r->u.fval = atan( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FABS:
		r->u.fval =  fabs( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FLOG10:
		if ( valv[0]->u.fval > 0 ) {
			r->u.fval = log10( valv[0]->u.fval );
		} else {
			sf_report( "Log10() of non positive number\n");
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FTANH:
		r->u.fval = tanh( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FACOS:
	{
		Float32		x;

		x = valv[0]->u.fval;

		if ( (x < 1.) && (x >-1.) ) {
			r->u.fval = acos( x );
		} else {
			sf_report( "Acos domain error\n" );
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	}

	case FC_FASIN:
	{
		Float32		x;

		x = valv[0]->u.fval;

		if ( (x < 1.) && (x >-1.) ) {
			r->u.fval = asin( x );
		} else {
			sf_report( "Asin domain error\n" );
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	}

	case FC_FTAN:
		r->u.fval = tan( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FSINH:
		r->u.fval = sinh( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FCOSH:
		r->u.fval = cosh( valv[0]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;


	case FC_FMOD:
		if ( valv[1]->u.fval != 0 ) {
			r->u.fval = fmod( valv[0]->u.fval, valv[1]->u.fval );
		} else {
			sf_report( "Mod with divisor of zero\n" );
			*err = R_MATH_ERROR;
			r->u.fval = 0;
		}
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FATAN2:
		r->u.fval = atan2( valv[0]->u.fval, valv[1]->u.fval );
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FSIGN:
		if ( valv[1]->u.fval > 0. ) {
			r->u.fval = fabs(valv[0]->u.fval);
		} else {
			r->u.fval = -fabs(valv[0]->u.fval);
		}
		r->d = dim_new( D_FLOAT );
		break;


	case FC_FINT:
		if ( fabs(valv[0]->u.fval) > FLOAT_INT_LIMIT ) {
			r->u.fval = valv[0]->u.fval;
		} else if ( valv[0]->u.fval > 0. ) {
			r->u.fval = floor( valv[0]->u.fval );
		} else {
			r->u.fval = ceil( valv[0]->u.fval );
		}
		r->d = dim_new( D_FLOAT );
		break;

	case FC_FFRAC:
		if ( fabs(valv[0]->u.fval)  < FLOAT_INT_LIMIT ) {
			r->u.fval = fmod( (double) valv[0]->u.fval, (double) 1. );
		} else {
			r->u.fval = 0.;
		}
		r->d = dim_new( D_FLOAT );
		break;


