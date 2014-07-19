/*
 *  qp_exe_op_vec.h  --
 *	Template for the vector numeric operations.
 *
 *  Original: 24-Nov-1995 22:11
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_op_vec.h,v 1.3 1996/04/23 18:38:34 maartenb Exp $
 *
 *  $Log: qp_exe_op_vec.h,v $
 *  Revision 1.3  1996/04/23 18:38:34  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_VADD + (OP_BASE):
{
	int		n, i, inc_o1;
	OP_TYPE		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];

	v = 0;
	for ( i=0 ; i < n ; i++ ) {
		v += *((OP_TYPE *) o1);
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
	break;
}

case FC_VMUL + (OP_BASE):
{
	int		n, i, inc_o1;
	OP_TYPE		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];

	v = 1;
	for ( i=0 ; i < n ; i++ ) {
		v *= *((OP_TYPE *) o1);
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
	break;
}

case FC_VMIN + (OP_BASE):
{
	int		n, i, inc_o1;
	OP_TYPE		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];

	v = OP_TYPE_MAX;
	for ( i=0 ; i < n ; i++ ) {
		if ( v > *((OP_TYPE *) o1) ) {
			v = *((OP_TYPE *) o1);
		}
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
	break;
}

case FC_VMAX + (OP_BASE):
{
	int		n, i, inc_o1;
	OP_TYPE		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[OP_DTYPE];

	v = OP_TYPE_MIN;
	for ( i=0 ; i < n ; i++ ) {
		if ( v < *((OP_TYPE *) o1) ) {
			v = *((OP_TYPE *) o1);
		}
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
	break;
}
