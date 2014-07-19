/*
 *  qp_exe_flow.h  --
 *	Interpreter switch for the flow control intructions
 *
 *  Original:  9-Jan-1995 16:23
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_flow.h,v 1.10 1996/04/23 18:38:28 maartenb Exp $
 *
 *  $Log: qp_exe_flow.h,v $
 *  Revision 1.10  1996/04/23 18:38:28  maartenb
 *  - Add RCS keywords
 *
 *
 */

{
	switch ( fc ) {
	case FC_NOP:
			break;

	case FC_HALT:
			/* keep in sync with FC_HALT_BIT in qp_execute.c */
			/* tricky, because we might have to pop several frames */
			while ( opc & FC_HALT_BIT ) {
				if ( cut_call_sp > 0 ) {
					int	size, i;
					bool	shape_pop;

					i = cut_index_stack[--cut_call_sp];
					opc = cut_opc_stack[cut_call_sp];
					pc = cut_call_stack[cut_call_sp];

					size = frame_size[framep];
					cut_size[i] = size;
					cut_bsize[i] = 4 * size *
						datatype_size[cut_type[i]];
					memcpy( cut_value[i], stack+frame[framep],
						cut_bsize[i] );
					cut_valid[i] = TRUE;
					if ( cut_has_shape[i] && ((opc & FC_DYNAMIC_INFO_BIT) == 0) ){
						SHAPE_POP( 1 );
					}
				} else {
					/* ready with main segment */
					running = FALSE;
					break;
				}
			}
			break;
	
	case FC_JUMP:
		{
			int		offset;

			offset = *pc++;
			pc += offset;
			break;
		}

	case FC_POP_JUMP:	/* pop on true, jump on false */
		{
			Int32		*ptr;
			int		offset;

			offset = *pc++;
			ptr =  stack + frame[framep];
			qp_assert( frame_size[framep] == 1 );
			if ( *ptr != 0 ) {
				POP_FRAME(1);
			} else {
				pc += offset;
			}
			break;
		}

	case FC_JUMP_POP:	/* pop on false, jump on true */
		{
			Int32		*ptr;
			int		offset;

			offset = *pc++;
			ptr =  stack + frame[framep];
			qp_assert( frame_size[framep] == 1 );
			if ( *ptr == 0 ) {
				POP_FRAME(1);
			} else {
				pc += offset;
			}
			break;
		}

	case FC_CUT:
		{
			register int	idx = *pc++;
			void	*p;

			if ( cut_valid[idx] ) {
				NEW_FRAME( cut_type[idx], cut_size[idx], p );
				memcpy( p, cut_value[idx], cut_bsize[idx] );
			} else {
				cut_index_stack[cut_call_sp] = idx;
				cut_opc_stack[cut_call_sp] = opc;
				cut_call_stack[cut_call_sp++] = pc;
				pc = cut_segment[idx];
				opc = 0; /* flush, to avoid early termination */
			}
			break;
		}
	
	case FC_GCUT_1D:
	{
		bool		*r;
		Float32		*o1, low, high;
		register int	i, n;
		int		cid = stack[frame[framep]];

		n = frame_size[framep-1];
		o1 = (Float32 *) &stack[frame[framep-1]];

		low = cut_data[cid]->points[0][0];
		high = cut_data[cid]->points[1][0];

		POP_FRAME(2);

		NEW_FRAME(D_BOOL,n,r);

		o1 += n - 1;
		r += n - 1;
		for ( i=0 ; i < n ; i++ ) {
			if ( low <= *o1 && *o1 < high ) {
				*r = TRUE;
			} else {
				*r = FALSE;
			}
			o1 -= 1;
			r -= 1;
		}
		break;
	}
	
	case FC_GCUT_2D:
	{
		register bool		*r;
		register Float32	*o1, *o2;
		Float32			tmp1, tmp2;
		int			i, n = 1, inc_o1, inc_o2;
		int			cid = stack[frame[framep]];

		if ( frame_size[framep-1] != 1 ) {
			o1 = (Float32 *)&stack[frame[framep-1]];
			inc_o1 = 1;
			n = frame_size[framep-1];
		} else {
			tmp1 = *((Float32 *) &stack[frame[framep-1]]);
			o1 = (Float32 *) &tmp1;
			inc_o1 = 0;
		}

		if ( frame_size[framep-2] != 1 ) {
			o2 = (Float32 *)&stack[frame[framep-2]];
			inc_o2 = 1;
			qp_assert( n==1 || n==frame_size[framep-2] );
			n = frame_size[framep-2];
		} else {
			tmp2 = *((Float32 *) &stack[frame[framep-2]]);
			o2 = (Float32 *) &tmp2;
			inc_o2 = 0;
		}
		POP_FRAME(3);

		NEW_FRAME(D_BOOL,n,r);

		o1 += inc_o1 * (n-1);
		o2 += inc_o2 * (n-1);
		r += (n-1);
		for ( i=0 ; i < n ; i++ ) {
			*r = polygon_sel( cid, *o2, *o1 );
			o1 -= inc_o1;
			o2 -= inc_o2;
			r -= 1;
		}
		break;
	}
	
	case FC_DUMP:	/* dump current stack frame */
		{
			Int32		*ptr;
			DataType	dt = ( opc>>13 ) & 0xF ;
			int		size, i;

			if ( framep <= 0 ) {
				printf( "DUMP: no frame ??? (framep = %d)\n",
					framep );
				break;
			}

			if ( ! exe_verbose ) {
				break;
			}

			ptr =  stack + frame[framep];
			size = frame_size[framep];
			if ( dt != frame_type[framep] ) {
				sf_report( "Warning: FC_DUMP - stack has %s"
					"i.s.o %s\n",
					datatype_to_str( frame_type[framep] ),
					datatype_to_str( dt ) );
			}

			printf( "DUMP: frame %d size %d type %s\n",
				framep, size, datatype_to_str( dt ) );
			
			for ( i=0; i < size ; i++ )
			switch ( dt ) {	/* switch DataType */
			case D_UNDEF:
					printf( "D_UNDEF\n" );
					break;
			case D_BOOL:
					printf( "%s\n", ptr[i] == 0 ?
						 ".false." : ".true." );
					break;
			case D_UINT:
					printf( "z'%x'\n", ptr[i] );
					break;
			case D_ULONG:
					sf_report( "FC_DUMP: D_ULONG not "
							"supported\n" );
					break;
			case D_INT:
					printf( "%d\n", ptr[i] );
					break;
			case D_LONG:
					sf_report( "FC_DUMP: D_LONG not "
							"supported\n" );
					break;
			case D_FLOAT:
					{
						char	b[32];
						Float32 f = ((float*)ptr)[i];

						sprintf( b, "%g", f );
						if ( ! strchr( b, '.' ) )
							strcat( b, "." );
						strcat( b, "\n" );
						fputs( b, stdout );
					}
					break;
			case D_DOUBLE:
					{
						char	b[32], *p;
						Float64 d = ((double*)ptr)[i];

						sprintf( b, "%lg", d );
						if ( ! strchr( b, '.' ) )
							strcat( b, "." );
						p = strchr( b, 'e' );
						if ( p == 0 ) {
							strcat( b, "d0" );
						} else {
							*p = 'd';
						}
						strcat( b, "\n" );
						fputs( b, stdout );
					}
					break;
			case D_STR:
					printf( "'%*.*s'\n", QP_STR_MAX,
						QP_STR_MAX, (char *)&ptr[i*8] );
					break;
			default:
				sf_report( "qp_exe_flow.h: FC_DUMP: "
					"unkown DataType (%d)\n", dt );
				*errp = R_INTERNAL_ERROR;
				running = FALSE;
				break;
			}

			if ( (opc & FC_DYNAMIC_INFO_BIT) != 0 ) {
				int	 i, n = shape_sp->ndim;

				fputs( "\t\tshape=(", stdout );
				for ( i = 0 ; i < n ; i++ ) {
					if ( i > 0 ) fputc( ',', stdout );
					fprintf( stdout, "%d", shape_sp->range[i] );
				}
				fputs( ")\n", stdout );
			}
		}
		break;
	
	case FC_CHECK_SHAPE:
		{
			bool		ok;

			SHAPE_LOAD(pc);
			SHAPE_CHECK_N( 2, ok );
			if ( ! ok ) {
				sf_report( "Evt %ld: Shapes of the expressions do "
					"not match.\n", ievt );
				running = FALSE;
				*errp = R_SHAPE_ERROR;
				break;	/* skip the event */
			}
			SHAPE_POP(2);
		}
		break;

	default:
		sf_report( "qp_exe_flow.h: Unkown Fcode ( %d )\n", fc );
		*errp = R_INTERNAL_ERROR;
		running = FALSE;
		break;
	}
}
