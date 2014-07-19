#include	<stdlib.h>
#include	<math.h>
#include	<string.h>

#include	"str.h"

#include	<cfortran/cfortran.h>

#include	"c_pawchn.h"
#include	"c_pawidn.h"
#include	"c_hcbook.h"
#include	"c_hcntpaw.h"
#include	"c_hcunit.h"
#include	"c_pcmatw.h"
#include	"c_pcaddr.h"
#include	"cern_types.h"
#include	<cfortran/comis.h>
#include	"errors.h"
#include	"fcode.h"
#include	"kuip_interface.h"
#include        <cfortran/hplot.h>
#include	<cfortran/hbook.h>
#include	<cfortran/higz.h>
#include	"paw_interface.h"
#include	"qp_command.h"
#include	"qp_dimension.h"
#include	"qp_execute.h"
#include	"qp_hbook_if.h"
#include	"qp_query.h"
#include	"qp_report.h"
#include	"qp_value.h"
#include	"qpflags.h"


#define Extern extern
#include        "qp_exec_global.h"
#include	"qp_exe_stack.h"	/* PRE_INCLUDE */

/* extern functions */
extern void qp_exec_seg( Segment, long, long, int *);

/* for backwards compatibility */
float
prot_log10( float x )
{
	if ( x > 0. ) {
		return (float)(log10( x ));
	} else {
		return -30.;
	}
}

void
qp_exec_evt(
	QueryExe *	qe,
	long		ievt,
        long            ievt_chain,
	QPCmd *		cmd,
	int *		errp
) {
	bool		register noexec;
	register int	i, j;


	/* initialisation for this event */

	STACK_INIT;


	for ( i=0 ; i < qe->ncuts ; i++ )
		cut_valid[i] = FALSE;

	/* execute possible selection expression */

	if ( qe->hasSel ) {
		qp_exec_seg( qe->sel, ievt, ievt_chain, errp );
		if ( *errp != R_NOERR ) {
			/* error, skip the event */
			return;
		}
	} else {
		/* put the default weight on the stack */
		float	*p;

		NEW_FRAME( D_FLOAT, 1, p );
		*p = 1.;
	}

	if ( frame_size[1] == 1 ) {
		if ( frame_type[1]  == D_FLOAT ) {
			noexec = *((Float32 *) &stack[frame[1]]) == 0.;
		} else {
			sf_report( "qp_exec_evt: Unexpected DataType %d\n", 
				frame_type[1] );
			noexec = TRUE;
		}
	} else {
		noexec = FALSE;	/* 0 length frame (==true) or vector */
	}

	for( i=0 ; i < qe->nexpr ; i++ ) {
		if ( noexec ) {
			/* produce empty stack frame */
			frame[framep+1] = frame[framep];
			framep += 1;
			frame_size[framep] = 0;
			frame_type[framep] = qe->expr_type[i];
		} else {
			qp_exec_seg( qe->expr[i], ievt, ievt_chain, errp );
			if ( *errp != R_NOERR ) {
				/* error, skip the event */
				return;
			}
		}
	}

	if ( !noexec && (qe->check_shape > 0) ) {
		bool		ok;

		SHAPE_CHECK_N( qe->check_shape, ok );
		if ( ! ok ) {
			*errp = R_SHAPE_ERROR;
			sf_report( "Evt %ld: Shapes of the expressions do not match.\n",
				ievt );
			return;	/* skip the event */
		}
	}

	switch ( cmd->command ) {	/* switch CmdType */
	case CMD_NULL:
		break;

	case CMD_MINMAX:

	{
		register Float32 *	o1;
		register QPCmdMinMax	*mm;
		void *			dp[MAX_EXPRS];
		int			inc[MAX_EXPRS];
		int			iframe, fb, n, inc_o1;
		char			key[MAX_LABEL_LEN+1];

		key[MAX_LABEL_LEN] = '\0';
				
		mm = &cmd->u.minmax;

		fb = framep - mm->n;
		o1 = (Float32 *) ( stack + frame[fb] );
		n = frame_size[fb];
		if ( n == 1 ) {
			inc_o1 = 0;
		} else {
			inc_o1 = 1;
		}

		for ( i=0 ; i < mm->n ; i++ ) {
			iframe = fb + i + 1;
			dp[i]  = stack + frame[iframe];
			if ( frame_size[iframe] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[iframe];
				} else {
					qp_assert( n == frame_size[iframe] );
				}
				inc[i] = 4 * datatype_size[frame_type[iframe]];
			} else {
				inc[i] = 0;
			}
		}

/* the big loop */

for ( j=n ; j > 0 ; j--, o1 += inc_o1 ) {
	for ( i=0 ; i < mm->n ; i++ ) {
		register Extreme *	ep;

		if ( *o1 != 0. ) {
			mm->has_events = TRUE;
			ep = &mm->el[i];
			switch ( ep->dtyp ) {
			case D_UINT: {
				UInt32  v = *(UInt32 *) dp[i];
				if (v < ep->min.uval) ep->min.uval = v;
				if (v > ep->max.uval ) ep->max.uval = v;
				break;
			}
			case D_ULONG: {
				UInt64  v = *(UInt64 *) dp[i];
				if (v < ep->min.luval) ep->min.luval=v;
				if (v > ep->max.luval) ep->max.luval=v;
				break;
			}
			case D_INT: {
				Int32  v = *(Int32 *) dp[i];
				if (v < ep->min.ival) ep->min.ival=v;
				if (v > ep->max.ival) ep->max.ival=v;
				break;
			}
			case D_LONG: {
				Int64  v = *(Int64 *) dp[i];
				if (v < ep->min.lival) ep->min.lival=v;
				if (v > ep->max.lival) ep->max.lival=v;
				break;
			}
			case D_FLOAT: {
				Float32  v = *(Float32 *) dp[i];
				if (v < ep->min.fval) ep->min.fval=v;
				if (v > ep->max.fval) ep->max.fval=v;
				break;
			}
			case D_DOUBLE: {
				Float64  v = *(Float64 *) dp[i];
				if (v < ep->min.lfval) ep->min.lfval=v;
				if (v > ep->max.lfval) ep->max.lfval=v;
				break;
			}
			case D_STR: {
				memcpy( key, dp[i], MAX_LABEL_LEN );
				smap_add( ep->sm, key );
				break;
			}
			case D_BOOL: {
				bool  b = *(bool *) dp[i];
				if ( b ) ep->max.bval=TRUE;
				if ( !b ) ep->min.bval=FALSE;
				break;
			}
			default: 
				qp_abort( "qp_exec_evt: Internal error. "
					"DataType %d unknown\n", ep->dtyp );
				break;
			}
		}
		dp[i] = (char *) dp[i] + inc[i];
	}
}
/* the end of the big loop */
	}
		break;

	case CMD_MINMAXAVG:

	{
		register Float32 *	o1;
		register QPCmdMinMaxAvg	*mm;
		void *			dp[MAX_EXPRS];
		int			inc[MAX_EXPRS];
		int			iframe, fb, n, inc_o1;
		char			key[MAX_LABEL_LEN+1];

		key[MAX_LABEL_LEN] = '\0';
				
		mm = &cmd->u.minmaxavg;

		fb = framep - mm->n;
		o1 = (Float32 *) ( stack + frame[fb] );
		n = frame_size[fb];
		if ( n == 1 ) {
			inc_o1 = 0;
		} else {
			inc_o1 = 1;
		}

		for ( i=0 ; i < mm->n ; i++ ) {
			iframe = fb + i + 1;
			dp[i]  = stack + frame[iframe];
			if ( frame_size[iframe] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[iframe];
				} else {
					qp_assert( n == frame_size[iframe] );
				}
				inc[i] = 4 * datatype_size[frame_type[iframe]];
			} else {
				inc[i] = 0;
			}
		}

/* the big loop */

for (j=n ; j > 0 ; j--, o1 += inc_o1 ) {
  if ( *o1 != 0. ) {
    mm->has_events = TRUE;
    mm->evt_count++;
    for ( i=0 ; i < mm->n ; i++ ) {
      register ExtremeAvg *	ep;
      
      ep = &mm->el[i];
      switch ( ep->dtyp ) {
      case D_UINT: {
	UInt32  v = *(UInt32 *) dp[i];
	if (v < ep->min.uval) ep->min.uval = v;
	if (v > ep->max.uval ) ep->max.uval = v;
	ep->avg += (double) v;
	break;
      }
      case D_ULONG: {
	UInt64  v = *(UInt64 *) dp[i];
	if (v < ep->min.luval) ep->min.luval=v;
	if (v > ep->max.luval) ep->max.luval=v;
	ep->avg += (double) v;
	break;
      }
      case D_INT: {
	Int32  v = *(Int32 *) dp[i];
	if (v < ep->min.ival) ep->min.ival=v;
	if (v > ep->max.ival) ep->max.ival=v;
	ep->avg += (double) v;
	break;
      }
      case D_LONG: {
	Int64  v = *(Int64 *) dp[i];
	if (v < ep->min.lival) ep->min.lival=v;
	if (v > ep->max.lival) ep->max.lival=v;
	ep->avg += (double) v;
	break;
      }
      case D_FLOAT: {
	Float32  v = *(Float32 *) dp[i];
	if (v < ep->min.fval) ep->min.fval=v;
	if (v > ep->max.fval) ep->max.fval=v;
	ep->avg += (double) v;
	break;
      }
      case D_DOUBLE: {
	Float64  v = *(Float64 *) dp[i];
	if (v < ep->min.lfval) ep->min.lfval=v;
	if (v > ep->max.lfval) ep->max.lfval=v;
	ep->avg += (double) v;
	break;
      }
      case D_STR: {
	memcpy( key, dp[i], MAX_LABEL_LEN );
	smap_add( ep->sm, key );
	break;
      }
      case D_BOOL: {
	bool  b = *(bool *) dp[i];
	if ( b ) ep->max.bval=TRUE;
	if ( !b ) ep->min.bval=FALSE;
	break;
      }
      default: 
	qp_abort( "qp_exec_evt: Internal error. "
		  "DataType %d unknown\n", ep->dtyp );
	break;
      }
    }
  }
  for ( i=0 ; i < mm->n ; i++ ) {
    dp[i] = (char *) dp[i] + inc[i];
  }
}


/* the end of the big loop */
	}
		break;

	case CMD_VFILL:
		break;

	case CMD_HFILL1:
		{
			register QPCmdHFill1	*h;
			register void		*o1;
			register Float32	*o2;
			register int		n, inc_o1, inc_o2;
			float			x, w;

			h = &cmd->u.hfill1;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_x;
			} else {
				inc_o1 = 0;
			}

			o2 = (Float32 *) &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = 1;
			} else {
				inc_o2 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o2 != 0. ) {
					h->x[h->n] =
						(*h->cvt_x.f)( o1, h->cvt_x.d );
					h->w[h->n] = *o2;
					h->n += 1;

					if ( h->n == HFILL_BSIZE ) {
						HF1N( h->idh, h->x,
							h->w, h->n );
						h->n = 0;
					}
				}

				o1 = (char *) o1 + inc_o1;
				o2 += inc_o2;
			}
		}
		break;

	case CMD_HFILL2:
		{
			register QPCmdHFill2	*h;
			register void		*o1;
			register void		*o2;
			register Float32	*o3;
			register int		n, inc_o1, inc_o2, inc_o3;
			float			x, y, w;

			h = &cmd->u.hfill2;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_x;
			} else {
				inc_o1 = 0;
			}

			o2 = &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = h->step_y;
			} else {
				inc_o2 = 0;
			}

			o3 = (Float32 *) &stack[frame[framep-2]];
			if ( frame_size[framep-2] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-2];
				} else {
					qp_assert( n == frame_size[framep-2] );
				}
				inc_o3 = 1;
			} else {
				inc_o3 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o3 != 0. ) {
					x = (*h->cvt_x.f )( o1, h->cvt_x.d );
					y = (*h->cvt_y.f )( o2, h->cvt_y.d );
					w = *o3;

					HFILL( h->idh, x, y, w );
				}

				o1 = (char *) o1 + inc_o1;
				o2 = (char *) o2 + inc_o2;
				o3 += inc_o3;
			}
		}
		break;

	case CMD_SCAT2:
		{
			register QPCmdScat2	*h;
			register void		*o1;
			register void		*o2;
			register Float32	*o3;
			register int		n, inc_o1, inc_o2, inc_o3;
			float			x, y, w;

			h = &cmd->u.scat2;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_x;
			} else {
				inc_o1 = 0;
			}

			o2 = &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = h->step_y;
			} else {
				inc_o2 = 0;
			}

			o3 = (Float32 *) &stack[frame[framep-2]];
			if ( frame_size[framep-2] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-2];
				} else {
					qp_assert( n == frame_size[framep-2] );
				}
				inc_o3 = 1;
			} else {
				inc_o3 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o3 != 0. ) {
					x = (*h->cvt_x.f )( o1, h->cvt_x.d );
					y = (*h->cvt_y.f )( o2, h->cvt_y.d );
					w = *o3;

					HFILL( h->idh, x, y, w );
					
					h->ievt[h->n] = ievt;
					if ( h->logx ) {
						h->x[h->n] = prot_log10( x );
					} else {
						h->x[h->n] = x;
					}
					if ( h->logy ) {
						h->y[h->n] = prot_log10( y );
					} else {
						h->y[h->n] = y;
					}
					h->n += 1;

					if ( h->n == HFILL_BSIZE ) {
						if ( h->line ) {
							IPL( h->n,
								h->x, h->y );
						} else {
							IPMID( h->n, h->x,
								h->y, 2,
								h->ievt);
						}
						h->n = 0;
					}
				}

				o1 = (char *) o1 + inc_o1;
				o2 = (char *) o2 + inc_o2;
				o3 += inc_o3;
			}
		}
		break;

	case CMD_SCAT3:
		{
			register QPCmdScat3	*h;
			register void		*o1;
			register void		*o2;
			register void		*o3;
			register Float32	*o4;
			register int		n;
			int			inc_o1, inc_o2, inc_o3, inc_o4;

			h = &cmd->u.scat3;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_x;
			} else {
				inc_o1 = 0;
			}

			o2 = &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = h->step_y;
			} else {
				inc_o2 = 0;
			}

			o3 = &stack[frame[framep-2]];
			if ( frame_size[framep-2] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-2];
				} else {
					qp_assert( n == frame_size[framep-2] );
				}
				inc_o3 = h->step_z;
			} else {
				inc_o3 = 0;
			}

			o4 = (Float32 *) &stack[frame[framep-3]];
			if ( frame_size[framep-3] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-3];
				} else {
					qp_assert( n == frame_size[framep-3] );
				}
				inc_o4 = 1;
			} else {
				inc_o4 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o4 != 0. ) {
					h->ievt[h->n] = ievt;
					if ( h->logx ) {
						h->x[h->n] = prot_log10(
						(*h->cvt_x.f)(o1, h->cvt_x.d) );
					} else {
						h->x[h->n] =
						(*h->cvt_x.f)(o1, h->cvt_x.d);
					}
					if ( h->logy ) {
						h->y[h->n] = prot_log10(
						(*h->cvt_y.f)(o2, h->cvt_y.d) );
					} else {
						h->y[h->n] =
						(*h->cvt_y.f)(o2, h->cvt_y.d);
					}
					if ( h->logz ) {
						h->z[h->n] = prot_log10(
						(*h->cvt_z.f)(o3, h->cvt_z.d) );
					} else {
						h->z[h->n] =
						(*h->cvt_z.f)(o3, h->cvt_z.d);
					}
					h->n += 1;

					if ( h->n == HFILL_BSIZE ) {
						if ( h->line ) {
							IPL3( h->n, h->x,
								h->y, h->z );
						} else {
							IPM3ID(h->n,h->x,
								h->y, h->z, 
								2, h->ievt);
						}
						h->n = 0;
					}
				}

				o1 = (char *) o1 + inc_o1;
				o2 = (char *) o2 + inc_o2;
				o3 = (char *) o3 + inc_o3;
				o4 += inc_o4;
			}

		}
		break;

	case CMD_GOURAUD:
		{
			register QPCmdGouraud	*h;
			register void		*o1;
			register void		*o2;
			register void		*o3;
			register Float32	*o4;
			register int		n;
			int			inc_o1, inc_o2, inc_o3, inc_o4;
			float                   x,y,z;
			int                     i,j,k;

			h = &cmd->u.gouraud;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_x;
			} else {
				inc_o1 = 0;
			}

			o2 = &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = h->step_y;
			} else {
				inc_o2 = 0;
			}

			o3 = &stack[frame[framep-2]];
			if ( frame_size[framep-2] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-2];
				} else {
					qp_assert( n == frame_size[framep-2] );
				}
				inc_o3 = h->step_z;
			} else {
				inc_o3 = 0;
			}

			o4 = (Float32 *) &stack[frame[framep-3]];
			if ( frame_size[framep-3] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-3];
				} else {
					qp_assert( n == frame_size[framep-3] );
				}
				inc_o4 = 1;
			} else {
				inc_o4 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o4 != 0. ) {
					if ( h->logx ) {
						x = prot_log10(
						(*h->cvt_x.f)(o1, h->cvt_x.d) );
					} else {
						x =
						(*h->cvt_x.f)(o1, h->cvt_x.d);
					}
					if ( h->logy ) {
						y = prot_log10(
						(*h->cvt_y.f)(o2, h->cvt_y.d) );
					} else {
						y =
						(*h->cvt_y.f)(o2, h->cvt_y.d);
					}
					if ( h->logz ) {
						z = prot_log10(
						(*h->cvt_z.f)(o3, h->cvt_z.d) );
					} else {
						z =
						(*h->cvt_z.f)(o3, h->cvt_z.d);
					}
					
					i = (int) (floor((x-(h->min[0]))/h->dx));
					if (i > (h->nx-1)) i = (h->nx)-1;
					j = (int) (floor((y-(h->min[1]))/h->dy));
					if (j > (h->ny-1)) j = (h->ny)-1;
					k = (int) (floor((z-(h->min[2]))/h->dz));
					if (k > (h->nz-1)) k = (h->nz)-1;
					
					h->value[i + j*h->nx + k*h->nx*h->ny] += 1;

				}

				o1 = (char *) o1 + inc_o1;
				o2 = (char *) o2 + inc_o2;
				o3 = (char *) o3 + inc_o3;
				o4 += inc_o4;
			}

		}
		break;

	case CMD_SCAT4:
		{
			register QPCmdScat4	*h;
			register void		*o1;
			register void		*o2;
			register void		*o3;
			register void		*o4;
			register Float32	*o5;
			register int		n;
			int			inc_o1, inc_o2, inc_o3,
						inc_o4, inc_o5;

			h = &cmd->u.scat4;

			o1 = &stack[frame[framep]];
			n = frame_size[framep];
			if ( n != 1 ) {
				inc_o1 = h->step_c;
			} else {
				inc_o1 = 0;
			}

			o2 = &stack[frame[framep-1]];
			if ( frame_size[framep-1] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-1];
				} else {
					qp_assert( n == frame_size[framep-1] );
				}
				inc_o2 = h->step_x;
			} else {
				inc_o2 = 0;
			}

			o3 = &stack[frame[framep-2]];
			if ( frame_size[framep-2] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-2];
				} else {
					qp_assert( n == frame_size[framep-2] );
				}
				inc_o3 = h->step_y;
			} else {
				inc_o3 = 0;
			}

			o4 = &stack[frame[framep-3]];
			if ( frame_size[framep-3] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-3];
				} else {
					qp_assert( n == frame_size[framep-3] );
				}
				inc_o4 = h->step_z;
			} else {
				inc_o4 = 0;
			}

			o5 = (Float32 *) &stack[frame[framep-4]];
			if ( frame_size[framep-4] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[framep-4];
				} else {
					qp_assert( n == frame_size[framep-4] );
				}
				inc_o5 = 1;
			} else {
				inc_o5 = 0;
			}

			for ( ; n > 0 ; n-- ) {
				if ( *o5 != 0. ) {
					h->ievt[h->n] = ievt;
					h->c[h->n] =
						(*h->cvt_c.f)(o1, h->cvt_c.d);
					if ( h->logx ) {
						h->x[h->n] = prot_log10(
						(*h->cvt_x.f)(o2, h->cvt_x.d) );
					} else {
						h->x[h->n] =
						(*h->cvt_x.f)(o2, h->cvt_x.d);
					}
					if ( h->logy ) {
						h->y[h->n] = prot_log10(
						(*h->cvt_y.f)(o3, h->cvt_y.d) );
					} else {
						h->y[h->n] =
						(*h->cvt_y.f)(o3, h->cvt_y.d);
					}
					if ( h->logz ) {
						h->z[h->n] = prot_log10(
						(*h->cvt_z.f)(o4, h->cvt_z.d) );
					} else {
						h->z[h->n] =
						(*h->cvt_z.f)(o4, h->cvt_z.d);
					}
					h->n += 1;

					if ( h->n == HFILL_BSIZE ) {
						if ( h->line ) {
							IPL3( h->n, h->x,
							h->y, h->z );
						} else {
							IPM4ID( h->n, 
							h->x, h->y,
							h->z, h->c,
							h->col_min, h->col_max,
							2, h->ievt);
						}
						h->n = 0;
					}
				}

				o1 = (char *) o1 + inc_o1;
				o2 = (char *) o2 + inc_o2;
				o3 = (char *) o3 + inc_o3;
				o4 = (char *) o4 + inc_o4;
				o5 += inc_o5;
			}
		}
		break;

	case CMD_LOOP:
		break;
	case CMD_DUMP:
	{
		register Float32 *	o1;
		void *			o2;
		int			iframe, fb, j, n, inc_o1, inc_o2;
		FILE *			fp = cmd->u.dump.fp;
		int			nexpr = qe->nexpr;
		DataType		dtyp;
		PrintFun		pf;
		char			pbuf[PRINTFUN_SIZE];

		fb = framep-nexpr;

		for ( i=0 ; i < nexpr ; i++ ) {

			if ( i != 0) {
				fputs( cmd->u.dump.sep2, fp );
			}

			o1 = (Float32 *) &stack[frame[fb]];
			n = frame_size[fb];
			if ( n == 1 ) {
				inc_o1 = 0;
			} else {
				inc_o1 = 1;
			}

			iframe = fb + i + 1;
			o2  = stack + frame[iframe];
			if ( frame_size[iframe] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[iframe];
				} else {
					qp_assert( n == frame_size[iframe] );
				}
				inc_o2 = 4*datatype_size[frame_type[iframe]];
			} else {
				inc_o2 = 0;
			}

			dtyp = frame_type[iframe];
			pf = cmd->u.dump.f[i];
			for ( j=0 ; j < n ; j++ ) {

				if ( j != 0) {
					fputs( cmd->u.dump.sep1, fp );
				}

				if ( *o1 != 0. ) {
					(*pf)( pbuf, o2 );
					fputs( pbuf, fp );
				}

				o1 += inc_o1;
				o2 = (char *) o2 + inc_o2;
			}
		}
		fputc( '\n', fp );
	}
	break;

	case CMD_SCAN:
	{
		QPCmdScan		*p;
		char			pbuf[PRINTFUN_SIZE];
		const char		*s;
		register Float32 *	o1;
		void *			ov[MAX_EXPRS];
		int			iframe, fb, j, n, fw;
		int			inc_o1, inc_ov[MAX_EXPRS];
		int			nexpr = qe->nexpr;
		bool			vflag;

		p = &cmd->u.scan;
		fb = framep-nexpr;

		o1 = (Float32 *) &stack[frame[fb]];
		n = frame_size[fb];
		if ( n == 1 ) {
			inc_o1 = 0;
		} else {
			inc_o1 = 1;
		}

		vflag = FALSE;
		for ( i=0 ; i < nexpr ; i++ ) {

			iframe = fb + i + 1;
			ov[i]  = stack + frame[iframe];
			if ( frame_size[iframe] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[iframe];
				} else {
					qp_assert( n == frame_size[iframe] );
				}
				inc_ov[i] = 4*datatype_size[frame_type[iframe]];
				vflag = TRUE;
			} else {
				inc_ov[i] = 0;
			}
		}

		if ( ! vflag ) n = 0;	/* only the first (scalar) line */

		if ( (inc_o1 != 0) || (*o1 != 0.) )
		for ( p->evt_count+=1, j = -1 ; j < n ; j++) {

			if ( (j != -1) && (*o1 == 0.) )  {
				for ( i=0 ; i < nexpr ; i++ ) {
					ov[i] = (char *) ov[i] + inc_ov[i];
				}
				o1 += inc_o1;
				continue;
			}

			if ( !p->pawpp ) {
				if ( p->cur_line == 0 ) {
					FTNPRN( p->sep );
					FTNPRN( p->title );
					FTNPRN( p->sep );
				}
				if ( j == -1 ) {
					sprintf( p->buf, "|%*ld%s|", ievt>999999 ?
						7 : 6, ievt, ievt>999999 ?
						"" : " " );
				} else {
					sprintf( p->buf, "| *%4d |", j+1 );
				}
			}


			for ( i=0 ; i < nexpr ; i++ ) {
				fw = p->fw[i] - 2;
				(*p->f[i])( pbuf, ov[i] );
				if ( p->pawpp ) {
					C2FCBSTR(pbuf,PCMATW.outstr[i],0);
				} else {
					sprintf( strchr(p->buf,0), " %-*.*s |", fw, fw, 
						(j==-1) ?
							(inc_ov[i]==0 ?pbuf:"")
						:
							(inc_ov[i]==0 ?"":pbuf)
						);
				}

				/* suppress increment for scalar line */
				if ( j >= 0 )
					ov[i] = (char *) ov[i] + inc_ov[i];
			}

			/* suppress increment for scalar line */
			if ( j >= 0 )
				o1 += inc_o1;

			if ( p->pawpp ) {
				int	i = ievt;
				/* How to do this in CFortran ? */
				(*p->matrow)( &i );
			} else {
				FTNPRN( p->buf );
				p->cur_line += 1;
				if ( !p->no_prompt &&
					(p->cur_line >= p->max_line) ) {
					FTNPRN( p->sep );
					p->cur_line = 0;
					s = ku_proc( "More...? ( <CR>/N/G )",
						NULL );
					if ( s[0]=='G' ) {
						p->no_prompt = TRUE;
					} else if ( s[0]=='N' || s[0]=='Q') {
						*errp = R_STOP;
						break;
					}
				}
			}
		}
	}
	break;

	case CMD_SPIDERSCAN:
	{
		QPCmdSpiderScan		*p;
		const char		*s;
		float                   fval[MAX_EXPRS];
		register Float32 *	o1;
		void *			ov[MAX_EXPRS];
		int			iframe, fb, j, n, fw;
		int			inc_o1, inc_ov[MAX_EXPRS];
		int			nexpr = qe->nexpr;
		bool			vflag;

		p = &cmd->u.sp_scan;
		fb = framep-nexpr;

		o1 = (Float32 *) &stack[frame[fb]];
		n = frame_size[fb];
		if ( n == 1 ) {
			inc_o1 = 0;
		} else {
			inc_o1 = 1;
		}

		vflag = FALSE;
		for ( i=0 ; i < nexpr ; i++ ) {

			iframe = fb + i + 1;
			ov[i]  = stack + frame[iframe];
			if ( frame_size[iframe] != 1 ) { /* allow 0 and > 1 */
				if ( n == 1 ) {
					n = frame_size[iframe];
				} else {
					qp_assert( n == frame_size[iframe] );
				}
				inc_ov[i] = 4*datatype_size[frame_type[iframe]];
				vflag = TRUE;
			} else {
				inc_ov[i] = 0;
			}
		}

		if ( ! vflag ) n = 0;	/* only the first (scalar) line */

		if ( (inc_o1 != 0) || (*o1 != 0.) )
		for ( p->evt_count+=1, j = -1 ; j < n ; j++) {

			if ( (j != -1) && (*o1 == 0.) )  {
				for ( i=0 ; i < nexpr ; i++ ) {
					ov[i] = (char *) ov[i] + inc_ov[i];
				}
				o1 += inc_o1;
				continue;
			}

			for ( i=0 ; i < nexpr ; i++ ) {
				fval[i] = (*p->cvt[i].f)(ov[i], p->cvt[i].d);
				/* suppress increment for scalar line */
				if ( j >= 0 )
					ov[i] = (char *) ov[i] + inc_ov[i];
			}

			/* suppress increment for scalar line */
			if ( j >= 0 )
				o1 += inc_o1;

			p->cur_line += 1;

			if (p->use_average)
			  PASPI(-(int)ievt, nexpr, p->expr_str, p->rzone, p->t_code, p->cur_line, fval, p->min, p->max, p->avg, p->spider_type);
			else
			  PASPI((int)ievt, nexpr, p->expr_str, p->rzone, p->t_code, p->cur_line, fval, p->min, p->max, p->avg, p->spider_type);
   
			if (p->cur_line >= p->max_line)  {
			  p->cur_line = 0;
			  IGTERM();
			  if ( !p->no_prompt) {
			    s = ku_proc( "More...? ( <CR>/N/G )", NULL );
			    if ( s[0]=='G' ) {
			      p->no_prompt = TRUE;
			    } else if ( s[0]=='N' || s[0]=='Q') {
			      *errp = R_STOP;
			      break;
			    }
			  } 
			}
		}
	}
	break;


	default:
		qp_abort( "Internal error, unknown CmdType %d", cmd->command );
		break;
	}
	}

