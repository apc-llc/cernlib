/*
 *  qp_exe_literal.h  --
 *
 *  Original: 15-Jan-1995 16:26
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_literal.h,v 1.4 1996/04/23 18:38:31 maartenb Exp $
 *
 *  $Log: qp_exe_literal.h,v $
 *  Revision 1.4  1996/04/23 18:38:31  maartenb
 *  - Add RCS keywords
 *
 *
 */

{
	Int32	*ptr;

	switch ( fc ) {

	case FC_LIT_SCA_BOOL:
		NEW_FRAME( D_BOOL, 1, ptr );
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_UINT:
		NEW_FRAME( D_UINT, 1, ptr );
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_ULONG:
		NEW_FRAME( D_ULONG, 1, ptr );
		*ptr++ = *pc++;
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_INT:
		NEW_FRAME( D_INT, 1, ptr );
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_LONG:
		NEW_FRAME( D_LONG, 1, ptr );
		*ptr++ = *pc++;
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_FLOAT:
		NEW_FRAME( D_FLOAT, 1, ptr );
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_DOUBLE:
		NEW_FRAME( D_DOUBLE, 1, ptr );
		*ptr++ = *pc++;
		*ptr = *pc++;
		break;

	case FC_LIT_SCA_STR:
		NEW_FRAME( D_STR, 1, ptr );
		(void) memcpy( ptr, pc, QP_STR_MAX );
		pc += 8;
		break;

	default:
		sf_report( "qp_exe_literal.h: Unkown Fcode ( %d )\n", fc );
		*errp = R_INTERNAL_ERROR;
		running = FALSE;
		break;
	}
}
