/*
 *  qp_exe_conv_case.h  --
 *
 *  Original: 23-Apr-1996 20:24
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_conv_case.h,v 1.4 1996/10/03 09:58:15 lecointe Exp $
 *
 *  $Log: qp_exe_conv_case.h,v $
 *  Revision 1.4  1996/10/03 09:58:15  lecointe
 *  Force the value of the C boolean TRUE to 1 (was -1 on DEC)
 *
 *  Revision 1.3  1996/04/23 18:38:25  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_B_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
	       	*((bool *) o1) = *((bool *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((bool *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((bool *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((bool *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((bool *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((bool *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((bool *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((bool *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((bool *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((bool *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
	       	*((Float32 *) o1) = *((bool *) o1) ? 1.0 : 0.0;
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((bool *) o1)[i] ? 1.0 : 0.0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_B_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((bool *) o1) ? 1.0 : 0.0;
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((bool *) o1)[i] ? 1.0 : 0.0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((UInt32 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((UInt32 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_U_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((UInt32 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((UInt32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((UInt64 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((UInt64 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LU_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((UInt64 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((UInt64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

/*
 *  #if 0
 */
case FC_I_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((Int32 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((Int32 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((Int32 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((Int32 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((Int32 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((Int32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((Int32 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((Int32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((Int32 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((Int32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((Int32 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((Int32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_I_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((Int32 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((Int32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((Int64 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((Int64 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LI_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((Int64 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((Int64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((Float32 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((Float32 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_F_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((Float32 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((Float32 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_B: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_BOOL );
		*((bool *) o1) = *((Float64 *) o1) ? 1 : 0;
	} else {
		NEW_FRAME( D_BOOL, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((bool *) r)[i] = ((Float64 *) o1)[i] ? 1 : 0;
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_U: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_UINT );
		*((UInt32 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_UINT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt32 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_LU: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_ULONG );
		*((UInt64 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_ULONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((UInt64 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_I: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_INT );
		*((Int32 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_INT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int32 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_LI: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_LONG );
		*((Int64 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_LONG, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Int64 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_F: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_FLOAT );
		*((Float32 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_FLOAT, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float32 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}

case FC_LF_2_LF: {
	int	i, n = frame_size[framep];
	o1 = &stack[frame[framep]];
	if ( n == 1 ) {
		SET_FRAME_TYPE( D_DOUBLE );
		*((Float64 *) o1) = *((Float64 *) o1);
	} else {
		NEW_FRAME( D_DOUBLE, n, r);
		for ( i=0 ; i < n ; i++ ) {
			((Float64 *) r)[i] = ((Float64 *) o1)[i];
		}
		SQUEEZE_FRAME(1);
	}
	break;
}
