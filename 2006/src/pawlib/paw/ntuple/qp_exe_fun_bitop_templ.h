/*
 *  qp_exe_fun_bitop_templ.h  --
 *
 *  Original: 27-Jan-1995 16:25
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_fun_bitop_templ.h,v 1.5 1996/04/23 18:38:28 maartenb Exp $
 *
 *  $Log: qp_exe_fun_bitop_templ.h,v $
 *  Revision 1.5  1996/04/23 18:38:28  maartenb
 *  - Add RCS keywords
 *
 *
 */
case FC_IOR + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp, o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(OP_TYPE *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(OP_TYPE);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) | *((OP_TYPE *) o2);
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}
	break;
}

case FC_IAND + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp, o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(OP_TYPE *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(OP_TYPE);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) & *((OP_TYPE *) o2);
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_INOT + (OP_BASE):
{
	register int		n;
	register OP_TYPE	*o1;

	n = frame_size[framep];
	o1 = (OP_TYPE *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		*((OP_TYPE *) o1) = ~ *((OP_TYPE *) o1);
	}
	break;
}


case FC_IEOR + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp, o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(OP_TYPE *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(OP_TYPE);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) ^ *((OP_TYPE *) o2);
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_BTEST + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(D_BOOL,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(bool) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((bool *) r) = ( *((OP_TYPE *) o1) & (1 << *((Int32 *) o2)) ) != 0;
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(bool);
	}

	break;
}


case FC_IBSET + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) | (1 << *((Int32 *) o2));
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_IBCLR + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) & ~ (1 << *((Int32 *) o2));
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_ISHFT + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) << *((Int32 *) o2);
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_ISHFTC + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2, *o3;
	int			o1_inc, o2_inc, o3_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp, o3_tmp;

	OP_TYPE			m, blob, mask;
	Int32			k, ic;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}
	if ( frame_size[framep-2] == 1 ) {
		o3_tmp = *(Int32 *) &stack[frame[framep-2]];
		o3 = (char *) &o3_tmp;
		o3_inc = 0;
	} else {
		o3 = (char *) &stack[frame[framep-2]];
		o3_inc = sizeof(Int32);
		n = frame_size[framep-2];
	}

	POP_FRAME(3);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	o3 += o3_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		m = (*(OP_TYPE *) o1);
		k = (*(Int32 *) o2);
		ic = (*(Int32 *) o3);

		if ( (ic <= 0) || (ic > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "ISHFT: arg 3 (%d) outside range [1..%d]\n",
				ic, 8 * sizeof( OP_TYPE ) );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}

		k = k % ic;
		k = (k + ic) % ic; /* k is now positive and less than ic */
		if ( ic == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << ic) - 1;
		}
		blob = m & mask;

		blob = ((blob >> (ic - k)) | (blob << k)) & mask;
		m = ( m & ~ mask ) | blob;

		*((OP_TYPE *) r) = m;

		o1 -= o1_inc;
		o2 -= o2_inc;
		o3 -= o3_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_IBITS + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2, *o3;
	int			o1_inc, o2_inc, o3_inc;
	OP_TYPE			o1_tmp;
	Int32			o2_tmp, o3_tmp;

	OP_TYPE			mask;
	Int32			i, len;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}
	if ( frame_size[framep-2] == 1 ) {
		o3_tmp = *(Int32 *) &stack[frame[framep-2]];
		o3 = (char *) &o3_tmp;
		o3_inc = 0;
	} else {
		o3 = (char *) &stack[frame[framep-2]];
		o3_inc = sizeof(Int32);
		n = frame_size[framep-2];
	}

	POP_FRAME(3);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	o3 += o3_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {

		i = (*(Int32 *) o2);
		len = (*(Int32 *) o3);

		if ( (i < 0) || (i >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "IBITS: arg 2 (%d) outside range [0..%d]\n",
				i, 8 * sizeof( OP_TYPE ) - 1 );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
		if ( (len <= 0) || (i+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "IBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 2 is %d)\n", len, 8 * sizeof( OP_TYPE )-i, i );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}

		if ( len == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << len) - 1;
		}

		*((OP_TYPE *) r) = ((*(OP_TYPE *) o1) >> i) & mask;

		o1 -= o1_inc;
		o2 -= o2_inc;
		o3 -= o3_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_MVBITS + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2, *o3, *o4, *o5;
	int			o1_inc, o2_inc, o3_inc, o4_inc, o5_inc;
	OP_TYPE			o1_tmp, o4_tmp;
	Int32			o2_tmp, o3_tmp, o5_tmp;

	OP_TYPE			mask;
	Int32			i, len, j;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(Int32 *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(Int32);
		n = frame_size[framep-1];
	}
	if ( frame_size[framep-2] == 1 ) {
		o3_tmp = *(Int32 *) &stack[frame[framep-2]];
		o3 = (char *) &o3_tmp;
		o3_inc = 0;
	} else {
		o3 = (char *) &stack[frame[framep-2]];
		o3_inc = sizeof(Int32);
		n = frame_size[framep-2];
	}
	if ( frame_size[framep-3] == 1 ) {
		o4_tmp = *(OP_TYPE *) &stack[frame[framep-3]];
		o4 = (char *) &o4_tmp;
		o4_inc = 0;
	} else {
		o4 = (char *) &stack[frame[framep-3]];
		o4_inc = sizeof(OP_TYPE);
		n = frame_size[framep-3];
	}
	if ( frame_size[framep-4] == 1 ) {
		o5_tmp = *(Int32 *) &stack[frame[framep-4]];
		o5 = (char *) &o5_tmp;
		o5_inc = 0;
	} else {
		o5 = (char *) &stack[frame[framep-4]];
		o5_inc = sizeof(Int32);
		n = frame_size[framep-4];
	}

	POP_FRAME(5);
	NEW_FRAME(OP_DTYPE,1,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	o3 += o3_inc * (n-1);
	o4 += o4_inc * (n-1);
	o5 += o5_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {

		i = (*(Int32 *) o2);
		len = (*(Int32 *) o3);
		j = (*(Int32 *) o5);

		if ( (i < 0) || (i >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 2 (%d) outside range [0..%d]\n",
				i, 8 * sizeof( OP_TYPE ) - 1 );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
		if ( (j < 0) || (j >= 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 5 (%d) outside range [0..%d]\n",
				j, 8 * sizeof( OP_TYPE ) - 1 );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
		if ( (len <= 0) || (i+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 2 is %d)\n", len, 8 * sizeof( OP_TYPE )-i, i );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
		if ( (j+len > 8 * sizeof( OP_TYPE )) ) {
			sf_report( "MVBITS: arg 3 (%d) outside range [1..%d] "
				"(arg 4 is %d)\n", len, 8 * sizeof( OP_TYPE )-j, j );
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}

		if ( len == (8 * sizeof( OP_TYPE )) ) {
			mask = ~ 0;
		} else {
			mask = (1 << len) - 1;
		}

		*((OP_TYPE *) r) = ((((*(OP_TYPE *) o1) >> i) & mask) << j)
				| ((*(OP_TYPE *) o4) & ~ (mask << j));

		o1 -= o1_inc;
		o2 -= o2_inc;
		o3 -= o3_inc;
		o4 -= o4_inc;
		o5 -= o5_inc;
		r -= sizeof(OP_TYPE);
	}

	break;
}


case FC_IABS + (OP_BASE):
{
	register int		n;
	register OP_TYPE	*o1;

	n = frame_size[framep];
	o1 = (OP_TYPE *) &stack[frame[framep]];
	for ( ; n > 0 ; n--, o1++ ) {
		if ( *((OP_TYPE *) o1) < 0 ) {
			*((OP_TYPE *) o1) = - *((OP_TYPE *) o1);
		}
	}
	break;
}


case FC_IMOD + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp, o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(OP_TYPE *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(OP_TYPE);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		if ( *((OP_TYPE *) o2) == 0 ) {
			sf_report( "Mod with divisor of zero\n" );
			*errp = R_MATH_ERROR;
			running = FALSE;
		} else {
			*((OP_TYPE *) r) = *((OP_TYPE *) o1) % *((OP_TYPE*) o2);
		}
		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}
	break;
}


case FC_ISIGN + (OP_BASE):
{
	register int		n;
	register char		*r, *o1, *o2;
	int			o1_inc, o2_inc;
	OP_TYPE			o1_tmp, o2_tmp;

	n = 1;
	if ( frame_size[framep] == 1 ) {
		o1_tmp = *(OP_TYPE *) &stack[frame[framep]];
		o1 = (char *) &o1_tmp;
		o1_inc = 0;
	} else {
		o1 = (char *) &stack[frame[framep]];
		o1_inc = sizeof(OP_TYPE);
		n = frame_size[framep];
	}
	if ( frame_size[framep-1] == 1 ) {
		o2_tmp = *(OP_TYPE *) &stack[frame[framep-1]];
		o2 = (char *) &o2_tmp;
		o2_inc = 0;
	} else {
		o2 = (char *) &stack[frame[framep-1]];
		o2_inc = sizeof(OP_TYPE);
		n = frame_size[framep-1];
	}

	POP_FRAME(2);
	NEW_FRAME(OP_DTYPE,n,r);

	o1 += o1_inc * (n-1);
	o2 += o2_inc * (n-1);
	r += sizeof(OP_TYPE) * (n-1);
	for ( ; n > 0 ; n-- ) {
		if (
			( ( *((OP_TYPE *) o1) < 0 && *((OP_TYPE *) o2) > 0 ) )
			|| 
			( ( *((OP_TYPE *) o1) > 0 && *((OP_TYPE *) o2) < 0 ) )
		) {
			*((OP_TYPE *) r) =  - *((OP_TYPE *) o1);
		} else {
			*((OP_TYPE *) r) = *((OP_TYPE *) o1);
		}

		o1 -= o1_inc;
		o2 -= o2_inc;
		r -= sizeof(OP_TYPE);
	}
	break;
}

