/*
 *  qp_eval_fun_double_math.h  --
 *	Implement the math funciotns for double precision.
 *
 *  Original:  8-Jan-1995 15:20
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_fun_double_math.h,v 1.8 1996/05/13 08:19:06 maartenb Exp $
 *
 *  $Log: qp_eval_fun_double_math.h,v $
 *  Revision 1.8  1996/05/13 08:19:06  maartenb
 *  - Removed remaining warning message about pow() operator.
 *
 *  Revision 1.7  1996/05/13 07:53:53  maartenb
 *  - Fix domain checking for log() and log10().
 *
 *  Revision 1.6  1996/04/23 18:38:20  maartenb
 *  - Add RCS keywords
 *
 *
 */
	case FC_DSIN:
		r->u.lfval = sin( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DCOS:
		r->u.lfval = cos( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DSQRT:
		if ( valv[0]->u.lfval >= 0 ) {
			r->u.lfval = sqrt( valv[0]->u.lfval );
		} else {
			sf_report( "Sqrt of negative number\n" );
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DEXP:
		r->u.lfval = exp( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DLOG:
		if ( valv[0]->u.lfval > 0 ) {
			r->u.lfval = log( valv[0]->u.lfval );
		} else {
			sf_report( "Log() of non positive number\n");
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DATAN:
		r->u.lfval = atan( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DABS:
		r->u.lfval =  fabs( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DLOG10:
		if ( valv[0]->u.lfval > 0 ) {
			r->u.lfval = log10( valv[0]->u.lfval );
		} else {
			sf_report("Log10() of non positive number\n");
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DTANH:
		r->u.lfval = tanh( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DACOS:
	{
		Float64		x;

		x = valv[0]->u.lfval;

		if ( (x < 1.) && (x >-1.) ) {
			r->u.lfval = acos( x );
		} else {
			sf_report( "Acos domain error\n" );
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	}

	case FC_DASIN:
	{
		Float64		x;

		x = valv[0]->u.lfval;

		if ( (x < 1.) && (x >-1.) ) {
			r->u.lfval = asin( x );
		} else {
			sf_report( "Asin domain error\n" );
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	}

	case FC_DTAN:
		r->u.lfval = tan( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DSINH:
		r->u.lfval = sinh( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DCOSH:
		r->u.lfval = cosh( valv[0]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;


	case FC_DMOD:
		if ( valv[1]->u.lfval != 0 ) {
			r->u.lfval = fmod( valv[0]->u.lfval, valv[1]->u.lfval );
		} else {
			sf_report( "Mod with divisor of zero\n" );
			*err = R_MATH_ERROR;
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DATAN2:
		r->u.lfval = atan2( valv[0]->u.lfval, valv[1]->u.lfval );
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DSIGN:
		if ( valv[1]->u.lfval > 0. ) {
			r->u.lfval = fabs(valv[0]->u.lfval);
		} else {
			r->u.lfval = -fabs(valv[0]->u.lfval);
		}
		r->d = dim_new( D_DOUBLE );
		break;


	case FC_DINT:
		if ( fabs(valv[0]->u.lfval) > DOUBLE_INT_LIMIT ) {
			r->u.lfval = valv[0]->u.lfval;
		} else if ( valv[0]->u.lfval > 0. ) {
			r->u.lfval = floor( valv[0]->u.lfval );
		} else {
			r->u.lfval = ceil( valv[0]->u.lfval );
		}
		r->d = dim_new( D_DOUBLE );
		break;

	case FC_DFRAC:
		if ( fabs(valv[0]->u.lfval) < DOUBLE_INT_LIMIT ) {
			r->u.lfval = fmod( (double) valv[0]->u.lfval, (double) 1. );
		} else {
			r->u.lfval = 0;
		}
		r->d = dim_new( D_DOUBLE );
		break;
