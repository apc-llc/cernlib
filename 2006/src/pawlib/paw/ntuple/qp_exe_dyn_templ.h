/*
 *  qp_exe_dyn_templ.h  --
 *
 *  Original:  4-Apr-1995 15:50
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_dyn_templ.h,v 1.8 1996/05/13 16:23:40 maartenb Exp $
 *
 *  $Log: qp_exe_dyn_templ.h,v $
 *  Revision 1.8  1996/05/13 16:23:40  maartenb
 *  - Add 4+ dimension dynamic array access.
 *
 *  Revision 1.7  1996/05/06 09:18:08  maartenb
 *  - Introduce a consistent reporting of array bound errors
 *
 *  - Cleanup dynamic non scalar access.
 *
 *  Revision 1.6  1996/04/26 09:09:11  maartenb
 *  - Implemented dynamic access to 3dim arrays.
 *
 *  Revision 1.5  1996/04/23 18:38:27  maartenb
 *  - Add RCS keywords
 *
 *
 */

/* 1 dimensional case */
case FC_CWN_DYN_BOOL + (OP_BASE):
		{
			int	i, stride, start, end, max;
			OP_TYPE	*ptr;

			stride = *pc++;	/* stride is always one for 1 dim */
					/* but we leave it in for regularity */
			max = *pc++;
			if ( max <= 0 ) {
				/* variable length, get index variable */
				max = *CWNBlock[-max].p;
			}

			start = *pc++;
			if ( start == 0 ) {
				/* pop */
				start = stack[frame[framep]];
				POP_FRAME(1);
			}

			end = *pc++;
			if ( end == -1 ) {
				end = start;
			} else if ( end == -2 ) {
				end = max;
			} else if ( end == 0 ) {
				/* pop */
				end = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start ) {
				qp_report_bounds(ievt,np->name,1,start,end,max);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end > max ) {
				qp_report_bounds(ievt,np->name,1,start,end,max);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}

			if ( start <= end ) {
				/* put on stack */
				NEW_FRAME( OP_DTYPE, end - start + 1, ptr );
				for ( i=start-1 ; i < end ; i++ ) {
					*ptr++ = ((OP_TYPE *) np->p)[ i ];

				}

				if ( info_flag ) {
					SHAPE_PUSH_1( end - start + 1 );
				}
			} else {
				/* put empty frame on stack */
				NEW_FRAME( OP_DTYPE, 0, ptr );
				if ( info_flag ) {
					SHAPE_PUSH_1( 0 );
				}
			}
		}
		break;

/* 2 dimensional case */
case FC_CWN_DYN_BOOL + (OP_BASE) + 16:
		{
			int		i, j, info_mask;
			int		stride1, start1, end1, max1;
			int		stride2, start2, end2, max2;
			OP_TYPE		*ptr;

			info_mask = 0;
			stride1 = *pc++;/* stride is always one for 1 dim */
					/* but we leave it in for regularity */
			max1 = *pc++;
			if ( max1 <= 0 ) {
				/* variable length, get index variable */
				max1 = *CWNBlock[-max1].p;
			}

			start1 = *pc++;
			if ( start1 == 0 ) {
				/* pop */
				start1 = stack[frame[framep]];
				POP_FRAME(1);
			}

			end1 = *pc++;
			if ( end1 == -1 ) {
				end1 = start1;
				info_mask |= 1;
			} else if ( end1 == -2 ) {
				end1 = max1;
			} else if ( end1 == 0 ) {
				/* pop */
				end1 = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start1 ) {
				qp_report_bounds(ievt,np->name,1,start1,end1,max1);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end1 > max1 ) {
				qp_report_bounds(ievt,np->name,1,start1,end1,max1);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}


			stride2 = *pc++;/* stride is always one for 1 dim */
					/* but we leave it in for regularity */
			max2 = *pc++;
			if ( max2 <= 0 ) {
				/* variable length, get index variable */
				max2 = *CWNBlock[-max2].p;
			}

			start2 = *pc++;
			if ( start2 == 0 ) {
				/* pop */
				start2 = stack[frame[framep]];
				POP_FRAME(1);
			}

			end2 = *pc++;
			if ( end2 == -1 ) {
				end2 = start2;
				info_mask |= 2;
			} else if ( end2 == -2 ) {
				end2 = max2;
			} else if ( end2 == 0 ) {
				/* pop */
				end2 = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start2 ) {
				qp_report_bounds(ievt,np->name,2,start2,end2,max2);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end2 > max2 ) {
				qp_report_bounds(ievt,np->name,2,start2,end2,max2);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}

			if ( (start1<=end1) && max2 != 0 && (start2<=end2) ) {
				/* put on stack */
				NEW_FRAME( OP_DTYPE, (end1-start1+1)*(end2-start2+1), ptr );
				for ( j=start2-1 ; j < end2 ; j++ ) {
					for ( i=start1-1 ; i < end1 ; i++ ) {
						*ptr++ = ((OP_TYPE *) np->p)[ i + j * stride2 ];
					}
				}
			} else {
				/* put empty frame on stack */
				NEW_FRAME( OP_DTYPE, 0, ptr );
			}

			if ( info_flag ) {
				int		r1, r2;

				r1 = start1<=end1 ? end1-start1+1 : 0 ;
				r2 = start2<=end2 ? end2-start2+1 : 0 ;

				switch ( info_mask ) {
				case 0:
					SHAPE_PUSH_2( r1, r2 ); break;
				case 1:
					SHAPE_PUSH_1( r2 ); break;
				case 2:
					SHAPE_PUSH_1( r1 ); break;
				case 3:
					qp_abort( "scalar does have shape\n" );
					break;
				}
			}
		}
		break;

/* 3 dimensional case */
case FC_CWN_DYN_BOOL + (OP_BASE) + 32:
		{
			int		i, j, k, info_mask;
			int		stride1, start1, end1, max1;
			int		stride2, start2, end2, max2;
			int		stride3, start3, end3, max3;
			OP_TYPE		*ptr;

			info_mask = 0;
			stride1 = *pc++;/* stride is always one for 1 dim */
					/* but we leave it in for regularity */
			max1 = *pc++;
			if ( max1 <= 0 ) {
				/* variable length, get index variable */
				max1 = *CWNBlock[-max1].p;
			}

			start1 = *pc++;
			if ( start1 == 0 ) {
				/* pop */
				start1 = stack[frame[framep]];
				POP_FRAME(1);
			}

			end1 = *pc++;
			if ( end1 == -1 ) {
				end1 = start1;
				info_mask |= 1;
			} else if ( end1 == -2 ) {
				end1 = max1;
			} else if ( end1 == 0 ) {
				/* pop */
				end1 = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start1 ) {
				qp_report_bounds(ievt,np->name,1,start1,end1,max1);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end1 > max1 ) {
				qp_report_bounds(ievt,np->name,1,start1,end1,max1);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}


			stride2 = *pc++;

			max2 = *pc++;
			if ( max2 <= 0 ) {
				/* variable length, get index variable */
				max2 = *CWNBlock[-max2].p;
			}

			start2 = *pc++;
			if ( start2 == 0 ) {
				/* pop */
				start2 = stack[frame[framep]];
				POP_FRAME(1);
			}

			end2 = *pc++;
			if ( end2 == -1 ) {
				end2 = start2;
				info_mask |= 2;
			} else if ( end2 == -2 ) {
				end2 = max2;
			} else if ( end2 == 0 ) {
				/* pop */
				end2 = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start2 ) {
				qp_report_bounds(ievt,np->name,2,start2,end2,max2);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end2 > max2 ) {
				qp_report_bounds(ievt,np->name,2,start2,end2,max2);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}


			stride3 = *pc++;
			max3 = *pc++;
			if ( max3 <= 0 ) {
				/* variable length, get index variable */
				max3 = *CWNBlock[-max3].p;
			}

			start3 = *pc++;
			if ( start3 == 0 ) {
				/* pop */
				start3 = stack[frame[framep]];
				POP_FRAME(1);
			}

			end3 = *pc++;
			if ( end3 == -1 ) {
				end3 = start3;
				info_mask |= 4;
			} else if ( end3 == -2 ) {
				end3 = max3;
			} else if ( end3 == 0 ) {
				/* pop */
				end3 = stack[frame[framep]];
				POP_FRAME(1);
			}

			/* now check consistency */

			if ( 1 > start3 ) {
				qp_report_bounds(ievt,np->name,3,start3,end3,max3);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}
			if ( end3 > max3 ) {
				qp_report_bounds(ievt,np->name,3,start3,end3,max3);
				*errp = R_MATH_ERROR;
				running = FALSE;
				break;
			}

			if ( (start1<=end1) && (start2<=end2) &&
				max3 != 0 && (start3<=end3) ) {
				/* put on stack */
				NEW_FRAME( OP_DTYPE, (end1-start1+1)*(end2-start2+1)*(end3-start3+1), ptr );
				for ( k=start3-1 ; k < end3 ; k++ ) {
					for ( j=start2-1 ; j < end2 ; j++ ) {
						for ( i=start1-1 ; i < end1 ; i++ ) {
							*ptr++ =((OP_TYPE *)np->p)[ i + j * stride2 + k * stride3 ];
						}
					}
				}
			} else {
				/* put empty frame on stack */
				NEW_FRAME( OP_DTYPE, 0, ptr );
			}

			if ( info_flag ) {
				int		r1, r2, r3;

				r1 = start1<=end1 ? end1-start1+1 : 0 ;
				r2 = start2<=end2 ? end2-start2+1 : 0 ;
				r3 = start3<=end3 ? end3-start3+1 : 0 ;

				switch ( info_mask ) {
				case 0:
					SHAPE_PUSH_3( r1, r2, r3 ); break;
				case 1:
					SHAPE_PUSH_2( r2, r3 ); break;
				case 2:
					SHAPE_PUSH_2( r1, r3 ); break;
				case 3:
					SHAPE_PUSH_1( r3 ); break;
				case 4:
					SHAPE_PUSH_2( r1, r2 ); break;
				case 5:
					SHAPE_PUSH_1( r2 ); break;
				case 6:
					SHAPE_PUSH_1( r1 ); break;
				case 7:
					qp_abort( "scalar does have shape\n" );
					break;
				}
			}
		}
		break;

/* 4+ dimensional case */
case FC_CWN_DYN_BOOL + (OP_BASE) + 48:
{
	typedef struct _traverse_ {
		int start, end, max, stride, ii, base;
	} Traverse;

	register Traverse	*sp, *done;
	register OP_TYPE	*ptr;
	Traverse		st[MAX_DIMENSION+1];
	int			j, info[MAX_DIMENSION];
	int			ndim, frame_len;
	bool			empty;

	ndim = *pc++; /* get the number of dimensions */
	done = &st[ndim];

	j = 0; /* for the shape info */
	for ( sp = st ; sp < done ; sp++ ) {
		sp->stride = *pc++;/* stride is always 1 for 1 */
			/* but we leave it in for regularity */
		sp->max = *pc++;
		if ( sp->max <= 0 ) {
			/* var length, get index variable */
			sp->max = *CWNBlock[-sp->max].p;
		}

		sp->start = *pc++;
		if ( sp->start == 0 ) {
			/* pop */
			sp->start = stack[frame[framep]];
			POP_FRAME(1);
		}

		sp->end = *pc++;
		if ( sp->end == -1 ) {
			sp->end = sp->start;
		} else if ( sp->end == -2 ) {
			sp->end = sp->max;
			info[j++] = sp->end - sp->start + 1;
		} else if ( sp->end == 0 ) {
			/* pop */
			sp->end = stack[frame[framep]];
			POP_FRAME(1);
			info[j++] = sp->end - sp->start + 1;
		}

		/* now check consistency */

		if ( 1 > sp->start ) {
			qp_report_bounds(ievt,np->name,(sp-st)+1,
				sp->start,sp->end,sp->max);
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
		if ( sp->end > sp->max ) {
			qp_report_bounds(ievt,np->name,(sp-st)+1,
				sp->start,sp->end,sp->max);
			*errp = R_MATH_ERROR;
			running = FALSE;
			break;
		}
	}

	empty = FALSE;
	frame_len = 1;
	for ( sp = st ; sp < done ; sp++ ) {
		if ( sp->start > sp->end ) {
			empty = TRUE;
			break;
		}
		if ( sp == done-1 && sp->max == 0 ) {
			empty = TRUE;
			break;
		}

		frame_len *= sp->end - sp->start + 1;

	}


	if ( ! empty ) {

		/* put on stack */
		NEW_FRAME( OP_DTYPE, frame_len, ptr );
		done->base = 0;
		sp = done - 1;
		sp->ii = sp->start - 1;
		sp->base = sp[1].base + sp->ii*sp->stride;

		while ( 1 ) {
			if (  sp->ii < sp->end ) {
				if ( sp > st ) {
					sp -= 1;
					sp->ii = sp->start - 1;
					sp->base = sp[1].base +
						sp->ii*sp->stride;
				} else {
					*ptr++ = ((OP_TYPE *)np->p)[sp->base];
					sp->ii += 1;
					sp->base += sp->stride;
				}
			} else {
				sp->ii = sp->start - 1;
				sp->base = sp->base + sp->ii*sp->stride;
				sp += 1;
				if ( sp == done ) break;
				sp->ii += 1;
				sp->base += sp->stride;
			}
		}

	} else {
		/* put empty frame on stack */
		NEW_FRAME( OP_DTYPE, 0, ptr );
	}

	if ( info_flag ) {
		SHAPE_PUSH_N( j, info );
	}
}
break;
