/*
 *  template_op_num.c  --
 *	Evaluation of numeric operators
 *
 *  Original: 25-Nov-1994 16:55
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_op_num.h,v 1.4 1997/01/21 16:43:57 couet Exp $
 *
 *  $Log: qp_eval_op_num.h,v $
 *  Revision 1.4  1997/01/21 16:43:57  couet
 *  - ** is now working for float constant also
 *
 *  Revision 1.3  1996/04/23 18:38:23  maartenb
 *  - Add RCS keywords
 *
 *
 */
	case FC_PLUS:
		r->u.OP_FIELD = valv[0]->u.OP_FIELD + valv[1]->u.OP_FIELD;
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_MINUS:
		r->u.OP_FIELD = valv[0]->u.OP_FIELD - valv[1]->u.OP_FIELD;
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_TIMES:
		r->u.OP_FIELD = valv[0]->u.OP_FIELD * valv[1]->u.OP_FIELD;
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_DIV:
		if ( valv[1]->u.OP_FIELD != 0 ) {
			r->u.OP_FIELD = valv[0]->u.OP_FIELD /
						valv[1]->u.OP_FIELD;
		} else {
			sf_report( "eval_template: divide by zero\n" );
			*err = R_MATH_ERROR;
			r->u.OP_FIELD = 0;
		}
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_POW:
		if ( valv[1]->d->dtyp == D_DOUBLE ) {
			r->u.fval = pow( valv[0]->u.fval, valv[1]->u.fval);
		} else if ( valv[1]->d->dtyp == D_FLOAT ) {
                        double value1;
                        double value2;
			value1 = (double)valv[0]->u.fval;
			value2 = (double)valv[1]->u.fval;
			r->u.fval = pow( value1, value2);
		} else {
			sf_report( "eval_template: power only for float or double\n" );
			*err = R_MATH_ERROR;
			r->u.OP_FIELD = 0;
		}
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_UMINUS:
		r->u.OP_FIELD = -valv[0]->u.OP_FIELD;
		r->d = dim_new( datatype_signed[OP_DTYPE] );
		break;

	case FC_MIN:
		if (  valv[0]->u.OP_FIELD <= valv[1]->u.OP_FIELD ) {
			r->u.OP_FIELD = valv[0]->u.OP_FIELD;
		} else {
			r->u.OP_FIELD = valv[1]->u.OP_FIELD;
		}
		r->d = dim_new( OP_DTYPE );
		break;

	case FC_MAX:
		if (  valv[0]->u.OP_FIELD <= valv[1]->u.OP_FIELD ) {
			r->u.OP_FIELD = valv[1]->u.OP_FIELD;
		} else {
			r->u.OP_FIELD = valv[0]->u.OP_FIELD;
		}
		r->d = dim_new( OP_DTYPE );
		break;

