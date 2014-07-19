/*
 *  qp_exe_op_num.h  --
 *	Template for the simple numeric operations.
 *	
 *  Original: 20-Jan-1995 16:57
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_op_num.h,v 1.4 1996/04/23 18:38:33 maartenb Exp $
 *
 *  $Log: qp_exe_op_num.h,v $
 *  Revision 1.4  1996/04/23 18:38:33  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_PLUS + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	*((OP_TYPE *) o2) = *((OP_TYPE *) o1) + *((OP_TYPE *) o2);
	break;

case FC_MINUS + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	*((OP_TYPE *) o2) = *((OP_TYPE *) o1) - *((OP_TYPE *) o2);
	break;

case FC_TIMES + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	*((OP_TYPE *) o2) = *((OP_TYPE *) o1) * *((OP_TYPE *) o2);
	break;

case FC_DIV + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	if ( *((OP_TYPE *) o2) == 0 ) {
		sf_report( "Divide by zero\n" );
		*errp = R_MATH_ERROR;
		running = FALSE;
		*((OP_TYPE *) o2) = 0;
	} else {
		*((OP_TYPE *) o2) = *((OP_TYPE *) o1) / *((OP_TYPE *) o2);
	}
	break;

case FC_POW + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	{
		double		x = *((OP_TYPE *) o1);
		double		y = *((OP_TYPE *) o2);
		double		dummy;

		if ( ( x == 0. && y <= 0. ) || ( x < 0. &&  modf(y,&dummy) != 0.  ) ) {
			sf_report( "Illegal values for power: %lg**%lg\n", x, y );
			*errp = R_MATH_ERROR;
			running = FALSE;
			*((OP_TYPE *) o2) = 0;
		} else {
			*((OP_TYPE *) o2) = pow( x, y );
		}
	}
	break;

case FC_UMINUS + (OP_BASE):		/* assume that signed and unsigned */
	o1 = &stack[frame[framep]];	/* have the same aligment ... */
	SET_FRAME_TYPE( datatype_signed[ OP_DTYPE ] );
	*((OP_TYPE *) o1) = - *((OP_TYPE *) o1);
	break;

case FC_MIN + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	if ( *((OP_TYPE *) o1) < *((OP_TYPE *) o2) ) {
		*((OP_TYPE *) o2) = *((OP_TYPE *) o1);
	}
	break;

case FC_MAX + (OP_BASE):
	o1 = &stack[frame[framep]];
	o2 = &stack[frame[framep-1]];
	POP_FRAME(1);
	if ( *((OP_TYPE *) o1) > *((OP_TYPE *) o2) ) {
		*((OP_TYPE *) o2) = *((OP_TYPE *) o1);
	}
	break;
