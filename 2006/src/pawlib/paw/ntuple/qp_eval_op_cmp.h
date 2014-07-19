/*
 *  qp_eval_op_cmp.h  --
 *	Evaluation of comparison operators
 *
 *  Original: 11-Jan-1995 15:11
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_eval_op_cmp.h,v 1.4 1996/04/23 18:38:22 maartenb Exp $
 *
 *  $Log: qp_eval_op_cmp.h,v $
 *  Revision 1.4  1996/04/23 18:38:22  maartenb
 *  - Add RCS keywords
 *
 *
 */

	case FC_LT:
		r->u.bval = valv[0]->u.OP_FIELD < valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LE:
		r->u.bval = valv[0]->u.OP_FIELD <= valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LTLT:
		r->u.bval = valv[0]->u.OP_FIELD < valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD < valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LTLE:
		r->u.bval = valv[0]->u.OP_FIELD < valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD <= valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LELT:
		r->u.bval = valv[0]->u.OP_FIELD <= valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD < valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_LELE:
		r->u.bval = valv[0]->u.OP_FIELD <= valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD <= valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GTGT:
		r->u.bval = valv[0]->u.OP_FIELD > valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD > valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GTGE:
		r->u.bval = valv[0]->u.OP_FIELD > valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD >= valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GEGT:
		r->u.bval = valv[0]->u.OP_FIELD >= valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD > valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GEGE:
		r->u.bval = valv[0]->u.OP_FIELD >= valv[1]->u.OP_FIELD 
			&& valv[1]->u.OP_FIELD >= valv[2]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GT:
		r->u.bval = valv[0]->u.OP_FIELD > valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_GE:
		r->u.bval = valv[0]->u.OP_FIELD >= valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;


	case FC_EQ:
		r->u.bval = valv[0]->u.OP_FIELD == valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_NE:
		r->u.bval = valv[0]->u.OP_FIELD != valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;

	case FC_CT:
		r->u.bval = valv[0]->u.OP_FIELD == valv[1]->u.OP_FIELD;
		r->d = dim_new( D_BOOL );
		break;
