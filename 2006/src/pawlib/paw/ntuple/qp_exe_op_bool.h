/*
 *  qp_exe_op_bool.h  --
 *
 *  Original: 24-Jan-1995 14:29
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_op_bool.h,v 1.3 1996/04/23 18:38:31 maartenb Exp $
 *
 *  $Log: qp_exe_op_bool.h,v $
 *  Revision 1.3  1996/04/23 18:38:31  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_OR:
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	*((bool *) o2) = *((bool *) o1) || *((bool *) o2);
	break;

case FC_AND:
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	*((bool *) o2) = *((bool *) o1) && *((bool *) o2);
	break;

case FC_NOT:
	o1 = &stack[frame[framep]];
	*((bool *) o1) = ! *((bool *) o1);
	break;

case FC_VAND:
{
	int		n, i, inc_o1;
	bool		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[D_BOOL];

	v = TRUE;
	for ( i=0 ; i < n ; i++ ) {
		v = v && *((bool *) o1);
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
}

case FC_VOR:
{
	int		n, i, inc_o1;
	bool		v;

	n = frame_size[framep];
	o1 = &stack[frame[framep]];
	inc_o1 = datatype_size[D_BOOL];

	v = FALSE;
	for ( i=0 ; i < n ; i++ ) {
		v = v || *((bool *) o1);
		o1 += inc_o1;
	}

	POP_FRAME(1);
	NEW_FRAME(OP_DTYPE,1,r);

	*((OP_TYPE *) r) = v;
	break;
}
