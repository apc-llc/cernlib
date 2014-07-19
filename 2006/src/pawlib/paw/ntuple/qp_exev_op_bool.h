/*
 *  qp_exev_op_bool.h  --
 *
 *  Original: 24-Jan-1995 14:29
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exev_op_bool.h,v 1.3 1996/04/23 18:38:38 maartenb Exp $
 *
 *  $Log: qp_exev_op_bool.h,v $
 *  Revision 1.3  1996/04/23 18:38:38  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_OR:
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
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
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) || *((OP_TYPE *) o2);
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_AND:
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	if ( frame_size[framep] != 1 ) {
		o1 = &stack[frame[framep]];
		inc_o1 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-1] );
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
		*((OP_TYPE *) r) = *((OP_TYPE *) o1) && *((OP_TYPE *) o2);
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_NOT:
{
	int		i, n = 1;

	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];
	n = frame_size[framep];

	o1 += inc_o1 * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		*((OP_TYPE *) o1) = ! *((OP_TYPE *) o1);
		o1 -= inc_o1;
	}
	break;
}
