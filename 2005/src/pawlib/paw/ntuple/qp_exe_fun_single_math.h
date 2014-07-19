/*
 *  qp_exe_fun_single_math.h  --
 *
 *  Original: 13-Feb-1995 15:23
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_fun_single_math.h,v 1.7 1996/05/09 10:21:15 maartenb Exp $
 *
 *  $Log: qp_exe_fun_single_math.h,v $
 *  Revision 1.7  1996/05/09 10:21:15  maartenb
 *  - Bugfix for two argument functions
 *
 *  Revision 1.6  1996/04/23 18:38:30  maartenb
 *  - Add RCS keywords
 *
 *
 */
case FC_FSIN:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = sin( *o1 );
	}
	break;
}

case FC_FCOS:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = cos( *o1 );
	}
	break;
}

case FC_FSQRT:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *o1 < 0. ) {
			sf_report( "Sqrt of negative number\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*o1 = sqrt( *o1 );
		}
	}
	break;
}

case FC_FEXP:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = exp( *o1 );
	}
	break;
}

case FC_FLOG:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *o1 <= 0. ) {
			sf_report( "Log of zero or negative number\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*o1 = log( *o1 );
		}
	}
	break;
}

case FC_FATAN:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = atan( *o1 );
	}
	break;
}

case FC_FABS:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = fabs( *o1 );
	}
	break;
}

case FC_FLOG10:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *o1 <= 0. ) {
			sf_report( "Log10 of zero or negative number\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*o1 = log10( *o1 );
		}
	}
	break;
}

case FC_FTANH:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = tanh( *o1 );
	}
	break;
}

case FC_FACOS:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *o1 < -1. || *o1 > 1.) {
			sf_report( "Acos domain error\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*o1 = acos( *o1 );
		}
	}
	break;
}

case FC_FASIN:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *o1 < -1. || *o1 > 1.) {
			sf_report( "Asin domain error\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*o1 = asin( *o1 );
		}
	}
	break;
}

case FC_FTAN:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = tan( *o1 );
	}
	break;
}

case FC_FSINH:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = sinh( *o1 );
	}
	break;
}

case FC_FCOSH:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*o1 = cosh( *o1 );
	}
	break;
}

case FC_FMOD:
{
	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(Float32 *) &stack[frame[framep]];
		o1 = &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (Float32 *) &stack[frame[framep]];
		o1_inc = 1;
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Float32 *) &stack[frame[framep-1]];
		o2 = &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (Float32 *) &stack[frame[framep-1]];
		o2_inc = 1;
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(D_FLOAT,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += (n-1);
	for ( ; n > 0 ; n-- ) {
		if ( *o2 == 0 ) {
			sf_report( "Mod with divisor of zero\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*r = fmod( *o1, *o2 );
		}
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= 1;
	}
	break;
}

case FC_FATAN2:
{
	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(Float32 *) &stack[frame[framep]];
		o1 = &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (Float32 *) &stack[frame[framep]];
		o1_inc = 1;
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Float32 *) &stack[frame[framep-1]];
		o2 = &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (Float32 *) &stack[frame[framep-1]];
		o2_inc = 1;
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(D_FLOAT,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += (n-1);
	for ( ; n > 0 ; n-- ) {
		*r = atan2( *o1, *o2 );
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= 1;
	}
	break;
}

case FC_FSIGN:
{
	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(Float32 *) &stack[frame[framep]];
		o1 = &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (Float32 *) &stack[frame[framep]];
		o1_inc = 1;
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Float32 *) &stack[frame[framep-1]];
		o2 = &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (Float32 *) &stack[frame[framep-1]];
		o2_inc = 1;
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(D_FLOAT,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += (n-1);
	for ( ; n > 0 ; n-- ) {
		if ( *o2 > 0. ) {
			*r = fabs( *o1 );
		} else {
			*r = -fabs( *o1 );
		}
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= 1;
	}
	break;
}

case FC_FINT:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( fabs( *o1 ) > FLOAT_INT_LIMIT ) {
			*o1 = *o1;
		} else if ( *o1 > 0. ) {
			*o1 = floor( *o1 );
		} else {
			*o1 = ceil( *o1 );
		}
	}
	break;
}

case FC_FFRAC:
{
	n = frame_size[framep];
	o1 = (Float32 *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( fabs( *o1 ) < FLOAT_INT_LIMIT ) {
			*o1 = fmod( (double) *o1, (double) 1. );
		} else {
			*o1 = 0.;
		}
	}
	break;
}
