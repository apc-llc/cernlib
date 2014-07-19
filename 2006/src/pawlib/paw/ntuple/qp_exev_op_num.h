/*
 *  qp_exev_op_num.h  --
 *	Template for the simple numeric operations, vector data.
 *	
 *  Original: 20-Jan-1995 16:57
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exev_op_num.h,v 1.7 1996/05/13 08:19:08 maartenb Exp $
 *
 *  $Log: qp_exev_op_num.h,v $
 *  Revision 1.7  1996/05/13 08:19:08  maartenb
 *  - Removed remaining warning message about pow() operator.
 *
 *  Revision 1.6  1996/04/23 18:38:41  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_PLUS + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) + *((OP_TYPE *) o2);
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_MINUS + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) - *((OP_TYPE *) o2);
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_TIMES + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) * *((OP_TYPE *) o2);
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_DIV + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o2) == 0 ) {
			sf_report( "Divide by zero\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		} else {
			*((OP_TYPE *) r) = *((OP_TYPE *) o1) / *((OP_TYPE *) o2);
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}


case FC_POW + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		double		x = *((OP_TYPE *) o1);
		double		y = *((OP_TYPE *) o2);
		double		dummy;

		if ( ( x == 0. && y <= 0. ) || ( x < 0. &&  modf(y,&dummy) != 0.  ) ) {
			sf_report( "Illegal values for power: %lg**%lg\n", x, y );
			*errp = R_MATH_ERROR;
			running = FALSE;
			*((OP_TYPE *) r) = 0;
			break;
		} else {
			*((OP_TYPE *) r) = pow( x, y );
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_UMINUS + (OP_BASE):		/* assume that signed and unsigned */
{					/* have the same aligment ... */
	int		i, n = 1;

	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];
	n = frame_size[framep];
	SET_FRAME_TYPE( datatype_signed[ OP_DTYPE ] );

	o1 += inc_o1 * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		*((OP_TYPE *) o1) = - *((OP_TYPE *) o1);
		o1 -= inc_o1;
	}
	break;
}

case FC_MIN + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) < *((OP_TYPE *) o2) ) {
			*((OP_TYPE *) r) = *((OP_TYPE *) o1);
		} else {
			*((OP_TYPE *) r) = *((OP_TYPE *) o2);
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_MAX + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		n = frame_size[framep];
	} else {
		tmp1 = *((OP_TYPE *) &stack[frame[framep]]);
		o1 = (Int32 *) &tmp1;
		inc_o1 = 0;
	}
	if ( frame_size[framep-1] != 1 ) {
		o2 = &stack[frame[framep-1]];
		inc_o2 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
		n = frame_size[framep-1];
	} else {
		tmp2 = *((OP_TYPE *) &stack[frame[framep-1]]);
		o2 = (Int32 *) &tmp2;
		inc_o2 = 0;
	}
	POP_FRAME(2);

	NEW_FRAME(OP_DTYPE,n,r);
	inc_r = datatype_size[OP_DTYPE];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) > *((OP_TYPE *) o2) ) {
			*((OP_TYPE *) r) = *((OP_TYPE *) o1);
		} else {
			*((OP_TYPE *) r) = *((OP_TYPE *) o2);
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}
