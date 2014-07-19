/*
 *  template_op_bool.c  --
 *	Evaluation of logical operators
 *
 *  Original: 25-Nov-1994 17:03
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_op_bool.h,v 1.2 1996/04/23 18:38:21 maartenb Exp $
 *
 *  $Log: qp_eval_op_bool.h,v $
 *  Revision 1.2  1996/04/23 18:38:21  maartenb
 *  - Add RCS keywords
 *
 *
 */

	case FC_OR:
		r->u.bval = valv[0]->u.bval || valv[1]->u.bval;
		r->d = dim_new( D_BOOL );
		break;
	case FC_AND:
		r->u.bval = valv[0]->u.bval && valv[1]->u.bval;
		r->d = dim_new( D_BOOL );
		break;
	case FC_NOT:
		r->u.bval = ! valv[0]->u.bval;
		r->d = dim_new( D_BOOL );
		break;
