/*
 *  qp_exev_op_cmp.h  --
 *
 *  Original: 23-Jan-1995 17:17
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exev_op_cmp.h,v 1.5 1996/04/23 18:38:39 maartenb Exp $
 *
 *  $Log: qp_exev_op_cmp.h,v $
 *  Revision 1.5  1996/04/23 18:38:39  maartenb
 *  - Add RCS keywords
 *
 *
 */

case FC_LT + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( (*((OP_TYPE *) o1)) < (*((OP_TYPE *) o2)) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_LE + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) <= *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}


/*
 *  Combine these entries to shrink the case statement, to avoid 
 *  HP-C case statement bug :-(
 */
case FC_LTLT + (OP_BASE):
case FC_LTLE + (OP_BASE):
case FC_LELT + (OP_BASE):
case FC_LELE + (OP_BASE):
case FC_GTGT + (OP_BASE):
case FC_GTGE + (OP_BASE):
case FC_GEGT + (OP_BASE):
case FC_GEGE + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2, tmp3;
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
	if ( frame_size[framep-2] != 1 ) {
		o3 = &stack[frame[framep-2]];
		inc_o3 = datatype_size[OP_DTYPE];
		qp_assert( n==1 || n==frame_size[framep-2] );
		n = frame_size[framep-2];
	} else {
		tmp3 = *((OP_TYPE *) &stack[frame[framep-2]]);
		o3 = (Int32 *) &tmp3;
		inc_o3 = 0;
	}
	POP_FRAME(3);

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	o3 += inc_o3 * (n-1);
	r += inc_r * (n-1);

	switch ( fc ) {
	case FC_LTLT + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) < *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) < *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LTLE + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) < *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) <= *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LELT + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) <= *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) < *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_LELE + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) <= *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) <= *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GTGT + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) > *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) > *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GTGE + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) > *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) >= *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GEGT + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) >= *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) > *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	case FC_GEGE + (OP_BASE):
		for ( i=0 ; i < n ; i++ ) {
			if ( (*((OP_TYPE *) o1) >= *((OP_TYPE *) o2)) &&
				(*((OP_TYPE *) o2) >= *((OP_TYPE *) o3))
			) {
				*((bool *) r) = TRUE;
			} else {
				*((bool *) r) = FALSE;
			}
			o1 -= inc_o1; o2 -= inc_o2; o3 -= inc_o3; r -= inc_r;
		}
		break;
	default:
		qp_assert(0);
		break;
	}
	break;
}


case FC_GT + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) > *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_GE + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) >= *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}


case FC_EQ + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) == *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

case FC_NE + (OP_BASE):
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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) != *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}

/* this needs a specific implementation, probably ... :-( */
case FC_CT + (OP_BASE):
{
	OP_TYPE		tmp1, tmp2;
	int		i, n = 1;

	sf_report( "Warning: FC_CT not implemented properly\n" );

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

	NEW_FRAME(D_BOOL,n,r);
	inc_r = datatype_size[D_BOOL];

	o1 += inc_o1 * (n-1);
	o2 += inc_o2 * (n-1);
	r += inc_r * (n-1);
	for ( i=0 ; i < n ; i++ ) {
		if ( *((OP_TYPE *) o1) == *((OP_TYPE *) o2) ) {
			*((bool *) r) = TRUE;
		} else {
			*((bool *) r) = FALSE;
		}
		o1 -= inc_o1;
		o2 -= inc_o2;
		r -= inc_r;
	}
	break;
}
