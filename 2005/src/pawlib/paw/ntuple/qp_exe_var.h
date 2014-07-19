/*
 *  qp_exe_var.h  --
 *
 *  Original: 17-Jan-1995 12:02
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_var.h,v 1.7 1996/04/23 18:38:35 maartenb Exp $
 *
 *  $Log: qp_exe_var.h,v $
 *  Revision 1.7  1996/04/23 18:38:35  maartenb
 *  - Add RCS keywords
 *
 *
 */

{
	register RefCWN		*np;
	Int32			*ptr;
	int			index, size;
	bool			info_flag;

	index = *pc++;

	np = &CWNBlock[index];

	if ( (opc & FC_DYNAMIC_INFO_BIT) != 0 ) {
		info_flag = TRUE;
		SHAPE_LOAD(pc);
	}

	switch ( fc ) {

	case FC_CWN_SCA_BOOL:
		NEW_FRAME( D_BOOL, 1, ptr );
		*ptr = *np->p;
		break;

	case FC_CWN_SCA_UINT:
		NEW_FRAME( D_UINT, 1, ptr );
		*ptr = *np->p;
		break;

	case FC_CWN_SCA_ULONG:
		NEW_FRAME( D_ULONG, 1, ptr );
		*((UInt64 *)ptr) = *((UInt64 *)np->p);
		break;

	case FC_CWN_SCA_INT:
		NEW_FRAME( D_INT, 1, ptr );
		*ptr = *np->p;
		break;

	case FC_CWN_SCA_LONG:
		NEW_FRAME( D_LONG, 1, ptr );
		*((Int64 *)ptr) = *((Int64 *)np->p);
		break;

	case FC_CWN_SCA_FLOAT:
		NEW_FRAME( D_FLOAT, 1, ptr );
		*ptr = *np->p;
		break;

	case FC_CWN_SCA_DOUBLE:
		NEW_FRAME( D_DOUBLE, 1, ptr );
		*((Float64 *)ptr) = *((Float64 *)np->p);
		break;

	case FC_CWN_SCA_STR:
		NEW_FRAME( D_STR, 1, ptr );
		(void) memset( ptr, ' ', QP_STR_MAX );
		(void) memcpy( ptr, np->p, np->isize );
		break;


	case FC_CWN_MAT_BOOL:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_BOOL, size, ptr );
		(void) memcpy( ptr, np->p, size << 2 );
		break;

	case FC_CWN_MAT_UINT:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_UINT, size, ptr );
		(void) memcpy( ptr, np->p, size << 2 );
		break;

	case FC_CWN_MAT_ULONG:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_ULONG, size, ptr );
		(void) memcpy( ptr, np->p, size << 3 );
		break;

	case FC_CWN_MAT_INT:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_INT, size, ptr );
		(void) memcpy( ptr, np->p, size << 2 );
		break;

	case FC_CWN_MAT_LONG:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_LONG, size, ptr );
		(void) memcpy( ptr, np->p, size << 3 );
		break;

	case FC_CWN_MAT_FLOAT:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_FLOAT, size, ptr );
		(void) memcpy( ptr, np->p, size << 2 );
		break;

	case FC_CWN_MAT_DOUBLE:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_DOUBLE, size, ptr );
		(void) memcpy( ptr, np->p, size << 3 );
		break;

	case FC_CWN_MAT_STR:
		size = np->var_base;
		if ( np->varDim ) {
			size *= *CWNBlock[np->var_index].p ;
			if ( info_flag ) {
				SHAPE_ADJUST(*CWNBlock[np->var_index].p);
			}
		}
		NEW_FRAME( D_STR, size, ptr );
		(void) memset( ptr, ' ', size * QP_STR_MAX );
		{
			register int	i, n;
			register char	*source, *dest;

			n = np->isize;
			source = (char *) np->p;
			dest = (char *) ptr;
			for ( i = size ; i > 0 ; i-- ) {
				(void) memcpy( dest, source, n );
				dest += QP_STR_MAX;
				source += n;
			}
		}
		break;


	case FC_RWN_SCA_LOAD:
		qp_assert( 0 <= index && index < MAX_RWN_COLS );
		NEW_FRAME( D_FLOAT, 1, ptr );
		*(Float32 *)ptr = PAWIDN.x[index];
		break;
	
	default:
		sf_report( "qp_exe_var.h: Unkown Fcode ( %d )\n", fc );
		*errp = R_INTERNAL_ERROR;
		running = FALSE;
		break;
	}
}
