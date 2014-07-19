/*
 *  qp_segment.c  --
 *
 *  Original: 11-Jan-1995 15:38
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_segment.c,v 1.19 1996/05/14 12:23:32 maartenb Exp $
 *
 *  $Log: qp_segment.c,v $
 *  Revision 1.19  1996/05/14 12:23:32  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.18  1996/05/12 15:13:55  maartenb
 *  - Replaced the handling of variables. The handling of variables is now
 *  fully dynamic and there is no other limit than the commonblocks to the
 *  number of var's in a query.
 *
 *  Revision 1.17  1996/04/23 18:38:58  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<assert.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>


#include	"errors.h"
#include	"fcode.h"
#include	"qp_dimension.h"
#include	"qp_report.h"
#include	"qp_segment.h"
#include	"qp_value.h"



#define	INIT_SEG_SIZE	512
#define	INCR_SEG_SIZE	128


SegBuild *
new_segbuild( int * errp )
{
	SegBuild *	sbp;

	sbp = (SegBuild *) calloc( sizeof( SegBuild ), 1 );
	if ( sbp == 0 ) {
		sf_report( "new_segbuild: Allocation error\n" );
		*errp = R_ALLOC_ERROR;
		return 0;
	}

	sbp->seg = (Segment) calloc( sizeof(Int32), INIT_SEG_SIZE );
	if ( sbp->seg == 0 ) {
		sf_report( "new_segbuild: Allocation error\n" );
		free( sbp );
		*errp = R_ALLOC_ERROR;
		return 0;
	}

	sbp->ok = TRUE;
	sbp->n = INIT_SEG_SIZE;
	sbp->cur = sbp->seg;
	sbp->last_opc = 0;

	return sbp;
}


void
free_segbuild( SegBuild * sbp )
{
	int	i;

	if ( sbp->n > 0 )
		free( sbp->seg );

	if ( sbp->lt != 0 ) {

		for( i=0; i < sbp->lt->n ; i++ ) {
			if ( sbp->lt->tab[i].ref != 0 ) {
				free( sbp->lt->tab[i].ref );
			}
		}

		free( sbp->lt->tab );
		free( sbp->lt );
	}

	free( sbp );
}


Segment
segment_from_segbuild( SegBuild *sbp )
{
	Segment		s_old, s_new;
	int		size;

	assert( sbp->cur > sbp->seg );
	assert( sbp->ok );

	s_old = sbp->seg;
	size = sbp->cur - sbp->seg;


	s_new = (Segment) malloc( size * sizeof(Int32) );
	qp_assert( s_new != 0 );

	memcpy( s_new, s_old, size * sizeof(Int32) );

	free_segbuild( sbp );


	return s_new;
}


bool
qp_seg_flag_opc( SegBuild * sbp, Int32 v, int * errp )
{
	if ( sbp->last_opc == 0 ) {
		sbp->ok = FALSE;
	}

	if ( sbp->ok ) {
		*sbp->last_opc |= v;
	}

	return sbp->ok;
}


bool
qp_seg_emit_opc( SegBuild * sbp, Int32 v, int * errp )
{
	bool		ok;

	ok = qp_seg_emit_word( sbp, v, errp );

	if ( ok ) {
		sbp->last_opc = sbp->cur - 1;
	}

	return ok;
}


bool
qp_seg_emit_word( SegBuild * sbp, Int32 v, int * errp )
{
	bool	ok = sbp->ok;
	int	newsize;

	if ( ok && (&sbp->seg[sbp->n] == sbp->cur) ) {
		/* grow the segment */

		newsize = sbp->n+INCR_SEG_SIZE;
		sbp->seg = (Segment) realloc( sbp->seg, newsize );	

		if ( sbp->seg == 0 ) {
			sf_report( "qp_seg_emit_word: Allocation error\n" );
			*errp = R_ALLOC_ERROR;
			ok = FALSE;
		} else {
			sbp->cur = &sbp->seg[sbp->n];
			sbp->n = newsize;
		}
	}

	if ( ok ) {
		*sbp->cur = v;
		sbp->cur += 1;
	}

	sbp->ok = ok;
	return ok;
}


int
qp_seg_new_label( SegBuild * sbp, int *errp )
{
	LabelTab	*lt;
	int		lid, i;

	if ( ! sbp->ok ) {
		if ( *errp == R_NOERR ) {	/* asure we return an error code */
			*errp = R_INTERNAL_ERROR;
		}
		return -1;
	}

	if ( sbp->lt == 0 ) {
		sbp->lt = (LabelTab *) calloc( sizeof(LabelTab), 1 );
		if ( sbp->lt == 0 ) {
			sbp->ok = FALSE;
			*errp = R_ALLOC_ERROR;
			sf_report( "qp_seg_new_label: Allocation error\n" );
			return -1;
		}
		sbp->lt->tab = (Label *) calloc( sizeof(Label), 5 );
		if ( sbp->lt->tab == 0 ) {
			sbp->ok = FALSE;
			*errp = R_ALLOC_ERROR;
			sf_report( "qp_seg_new_label: Allocation error\n" );
			return -1;
		}
		sbp->lt->size = 5;
	}

	lt = sbp->lt;

	if ( lt->n == lt->size ) {
		lt->size += 5;
		lt->tab = (Label *) realloc( lt->tab, sizeof(Label) * lt->size );
		if ( lt->tab == 0 ) {
			sbp->ok = FALSE;
			*errp = R_ALLOC_ERROR;
			sf_report( "qp_seg_new_label: Allocation error\n" );
			return -1;
		}
		for( i=lt->n ; i < lt->size ; i++ ) {
			lt->tab[i].addr = 0;
			lt->tab[i].n = 0;
			lt->tab[i].size = 0;
			lt->tab[i].ref = 0;
		}
	}

	lid = lt->n;
	lt->n += 1;

	return lid;
}


bool
qp_seg_labels_resolved( SegBuild * sbp, int *errp )
{
	Segment		cur, addr;
	LabelTab	*lt;
	int		i;

	if ( ! sbp->ok ) {
		if ( *errp == R_NOERR ) {	/* asure we return an error code */
			*errp = R_INTERNAL_ERROR;
		}
		return FALSE;
	}

	lt = sbp->lt;

	if ( lt == 0 ) {	/* no labels yet */
		return TRUE;
	}

	cur = sbp->cur;
	for ( i=0 ; i < lt->n ; i++ ) {
		addr = lt->tab[i].addr;
		if ( (addr == 0) || (addr == cur) ) {
			return FALSE;
		}
	}

	return TRUE;
}


bool
qp_seg_emit_ref( SegBuild * sbp, int lid, int *errp )
{
	Label		*l;

	if ( ! sbp->ok ) {
		return FALSE;
	}

	assert( (sbp->lt != 0) && (0 <= lid) && (lid < sbp->lt->n) );

	l = &sbp->lt->tab[ lid ];
	if ( l->addr != 0 ) {
		qp_seg_emit_word( sbp,
			l->addr - sbp->cur - 1,
			errp );
	} else {
		/* store the position and fill in later */
		/* when addr becomes known */
		if ( l->n == l->size ) {
			l->size += 5;
			if ( l->ref == 0 ) {
				l->ref = (Segment *) malloc( sizeof(Segment) * l->size );
			} else {
				l->ref = (Segment *) realloc( l->ref,
					sizeof(Segment) * l->size );
			}
			if ( l->ref == 0 ) {
				*errp = R_ALLOC_ERROR;
				sf_report("qp_seg_emit_ref: Allocation error\n");
				sbp->ok = FALSE;
				return FALSE;
			}
		}
		l->ref[l->n] = sbp->cur;
		l->n += 1;
		sbp->cur += 1;
	}

	return TRUE;
}


bool
qp_seg_emit_label( SegBuild * sbp, int lid, int *errp )
{
	int		i;
	Label		*l;

	if ( ! sbp->ok ) {
		return FALSE;
	}

	assert( (sbp->lt != 0) && (0 <= lid) && (lid < sbp->lt->n) );

	l = &sbp->lt->tab[ lid ];

	assert( l->addr == 0 );

	l->addr = sbp->cur;

	if ( l->n > 0 ) {	/* fill in recorded references */
		for ( i=0 ; i < l->n ; i++ ) {
			*l->ref[i] = l->addr - l->ref[i] - 1;
		}

		free( l->ref );
		l->ref = 0;
		l->n = 0;
		l->size = 0;
	}

	return TRUE;
}


static Segment		pc, pc0;
static int		pc_rel;


static char * bitop_names[] = {

	"IOR",
	"IAND",
	"INOT",
	"IEOR",
	"ISHFT",
	"ISHFTC",
	"IBITS",
	"MVBITS",
	"BTEST",
	"IBSET",
	"IBCLR",
	"IABS",
	"IMOD",
	"ISIGN"
};

static bool
dump_bitop(
	FILE *		fp,
	UInt32		opc
)
{
	bool		running = TRUE;
	DataType	dtype;
	int		i;

	/* fc + i * 16 ; i=0,1,2,3 for uint, ulong, int, long */

	i = (opc & MASK_FC ) - FC_IOR;
	dtype = ( i / 16 ) + D_UINT;
	i = i % 16;

	fprintf( fp, "%s <%s>\n", bitop_names[i],
		datatype_to_str( dtype ) );
	
	if ( (opc & FC_DYNAMIC_TEST_BIT) != 0 ) {
		fprintf( fp, "\t\tshape_check=%d\n", *pc++ );
	}

	return running;
}


static char * fun_names[] = {
	"SIN",
	"COS",
	"SQRT",
	"EXP",
	"LOG",
	"ATAN",
	"ABS",
	"LOG10",
	"TANH",
	"ACOS",
	"ASIN",
	"TAN",
	"SINH",
	"COSH",

	"MOD",
	"ATAN2",
	"SIGN",

	"INT",
};

static bool
dump_fun(
	FILE *		fp,
	UInt32		opc,
	DataType	dtype
)
{
	bool		running = TRUE;
	int		i;

	if ( dtype == D_FLOAT ) {
		i = (opc & MASK_FC ) - FC_FSIN;
	} else {
		i = (opc & MASK_FC ) - FC_DSIN;
	}

	fprintf( fp, "%s <%s>\n", fun_names[i],
		datatype_to_str( dtype ) );
	
	if ( (opc & FC_DYNAMIC_TEST_BIT) != 0 ) {
		fprintf( fp, "\t\tshape_check=%d\n", *pc++ );
	}

	return running;
}


static bool
dump_dyn(
	FILE *		fp,
	UInt32		opc,
	QueryExe *	qe
)
{
	FCode		fc;
	DataType	dtype;
	int		i, dim, stride, max, start, end;
	int		idx;
	Variable	v;

	fc = opc & MASK_FC;

	dim = (opc >> 4 ) & 0x3;

	fc -= 16 * dim;
	dtype = fc - FC_CWN_DYN_BOOL;

	idx = *pc++;

	if ( dim == 3 ) {
		dim = *pc++;
	}

	dim += 1;

	v = vvec_get( qe->vv, idx );
	fprintf( fp, "LOAD_DYN <%s> %s:%s (%d) dim=%d\n", 
		datatype_to_str( dtype ), v->block,
		v->name, idx, dim );
	
	for( i=0 ; i < dim ; i++ ) {
		stride = *pc++;
		max = *pc++;
		start = *pc++;
		end = *pc++;
		fprintf( fp, "\t\t{ stride=%d, max=%d, start=%d, end=%d }\n",
			stride, max, start, end );
	}

	return TRUE;
}


static bool
dump_comis(
	FILE *		fp,
	UInt32		opc
)
{
	bool		running = TRUE;
	int		iaddr, npar;
	DataType	dtype;

	iaddr = *pc++;
	npar = *pc++;

	switch ( opc & MASK_FC ) {

	case FC_CS_LFUN: dtype = D_BOOL; break;
	case FC_CS_IFUN: dtype = D_INT; break;
	case FC_CS_RFUN: dtype = D_FLOAT; break;
	case FC_CS_DFUN: dtype = D_DOUBLE; break;
	case FC_CS_SFUN: dtype = D_STR; break;

	default:
		sf_report( "dump_comis: Unkown Fcode ( %d )\n",
			opc & MASK_FC );
		running = FALSE;
		break;
	}

	fprintf( fp, "COMIS %d <%s> npar=%d\n", iaddr, 
		datatype_to_str( dtype ), npar );

	return running;
}


static bool
dump_var(
	FILE *		fp,
	UInt32		opc,
	QueryExe *	qe
)
{
	bool		running = TRUE;
	int		idx;
	DataType	dtype;
	char *		name;
	Variable	v;

	idx = *pc++;

	switch ( opc & MASK_FC ) {

	case FC_CWN_SCA_BOOL:
	case FC_CWN_SCA_UINT:
	case FC_CWN_SCA_ULONG:
	case FC_CWN_SCA_INT:
	case FC_CWN_SCA_LONG:
	case FC_CWN_SCA_FLOAT:
	case FC_CWN_SCA_DOUBLE:
	case FC_CWN_SCA_STR:
		dtype = (DataType) ((opc & MASK_FC) - FC_CWN_SCA_BOOL);
		name = "LOAD_SCA";
		break;

	case FC_CWN_MAT_BOOL:
	case FC_CWN_MAT_UINT:
	case FC_CWN_MAT_ULONG:
	case FC_CWN_MAT_INT:
	case FC_CWN_MAT_LONG:
	case FC_CWN_MAT_FLOAT:
	case FC_CWN_MAT_DOUBLE:
	case FC_CWN_MAT_STR:
		dtype = (DataType) ((opc & MASK_FC) - FC_CWN_MAT_BOOL);
		name = "LOAD_MAT";
		break;

	case FC_RWN_SCA_LOAD:
		dtype = D_FLOAT;
		name = "LOAD_RWN";
		break;

	default:
		sf_report( "dump_var: Unkown Fcode ( %d )\n",
			opc & MASK_FC );
		running = FALSE;
		break;
	}

	v = vvec_get( qe->vv, idx );
	fprintf( fp, "%s <%s> %s:%s (%d)\n", name, 
		datatype_to_str( dtype ), v->block,
		v->name, idx );
	
	if ( (opc & FC_DYNAMIC_INFO_BIT) != 0 ) {
		int	 i, n = *pc++;

		fputs( "\t\tshape=(", fp );
		for ( i = 0 ; i < n ; i++ ) {
			if ( i > 0 ) fputc( ',', fp );
			fprintf( fp, "%d", *pc++ );
		}
		fputs( ")\n", fp );
	}

	return running;
}


static bool
dump_literal(
	FILE *		fp,
	UInt32		opc
)
{
	bool		running = TRUE;
	int		offset, idx;
	Uval		val;

	switch ( opc & MASK_FC ) {

	case FC_LIT_SCA_BOOL:
		val.buf[0] = *pc++;
		fprintf( fp, "LITERAL %s\n", 
			val.bval ? ".true." : ".false." );
		break;

	case FC_LIT_SCA_UINT:
		val.buf[0] = *pc++;
		fprintf( fp, "LITERAL z'%x'\n", val.uval );
		break;

	case FC_LIT_SCA_ULONG:
		val.buf[0] = *pc++;
		val.buf[1] = *pc++;
		fprintf( fp, "LITERAL z'%lx'\n", val.luval );
		break;

	case FC_LIT_SCA_INT:
		val.buf[0] = *pc++;
		fprintf( fp, "LITERAL %d\n", val.ival );
		break;

	case FC_LIT_SCA_LONG:
		val.buf[0] = *pc++;
		val.buf[1] = *pc++;
		fprintf( fp, "LITERAL %ld\n", val.lival );
		break;

	case FC_LIT_SCA_FLOAT:
		val.buf[0] = *pc++;
		fprintf( fp, "LITERAL %g\n", val.fval );
		break;

	case FC_LIT_SCA_DOUBLE:
		val.buf[0] = *pc++;
		val.buf[1] = *pc++;
		fprintf( fp, "LITERAL %lg\n", val.lfval );
		break;

	case FC_LIT_SCA_STR:
		fprintf( fp, "LITERAL '%.*s'\n", QP_STR_MAX, (char *) pc );
		pc += 8;
		break;

	default:
		sf_report( "dump_literal: Unkown Fcode ( %d )\n",
			opc & MASK_FC );
		running = FALSE;
		break;
	}

	return running;
}


static bool
dump_mask(
	FILE *		fp,
	UInt32		opc,
	QueryExe *	qe
)
{
	bool		running = TRUE;
	int		idx, ibit;
	char		*mname;

	idx = *pc++;
	mname = qe->mask[idx]->mname;

	switch ( opc & MASK_FC ) {
	case FC_MASK_SET_BIT:
		ibit = *pc++;
		fprintf( fp, "SET_BIT %s(%d)\n", mname, ibit );
		break;

	case FC_MASK_GET_BIT:
		ibit = *pc++;
		fprintf( fp, "GET_BIT %s(%d)\n", mname, ibit );
		break;
	case FC_MASK_GET_WORD:
		fprintf( fp, "GET_WORD %s\n", mname );
		break;

	default:
		sf_report( "dump_flow: Unkown Fcode ( %d )\n",
			opc & MASK_FC );
		running = FALSE;
		break;
	}

	return running;
}


static bool
dump_flow(
	FILE *		fp,
	UInt32		opc
)
{
	bool		running = TRUE;
	int		offset, idx;

	switch ( opc & MASK_FC ) {

	case FC_HALT:
		fprintf( fp, "HALT\n" );
		running = FALSE;
		break;
	case FC_JUMP:
		offset = *pc++;
		fprintf( fp, "JUMP %d\n", pc+offset - pc0 );
		break;
	case FC_POP_JUMP:	/* pop on true, jump on false */
		offset = *pc++;
		fprintf( fp, "POP_JUMP %d\n", pc+offset - pc0 );
		break;
	case FC_JUMP_POP:	/* pop on false, jump on true */
		offset = *pc++;
		fprintf( fp, "JUMP_POP %d\n", pc+offset - pc0 );
		break;

	case FC_NOP:
		fprintf( fp, "NOP\n" );
		break;

	case FC_DUMP:
		{
			DataType	dt = ( opc>>13 ) & 0xF ;
		
			fprintf( fp, "DUMP <%s>\n", datatype_to_str( dt ) );
		}
		break;

	case FC_CHECK_SHAPE:	/* push a static shape on the shape stack */
		{
			int	 i, n = *pc++;

			fprintf( fp, "CHECK_SHAPE\n" );
			fputs( "\t\tshape=(", fp );
			for ( i = 0 ; i < n ; i++ ) {
				if ( i > 0 ) fputc( ',', fp );
				fprintf( fp, "%d", *pc++ );
			}
			fputs( ")\n", fp );
		}
		break;

	case FC_CUT:		/* Call an expression CUT */
		idx = *pc++;
		fprintf( fp, "CUT #%d\n", idx );
		break;

	case FC_GCUT_1D:	/* Call an 1d graphical CUT */
		fprintf( fp, "GCUT_1D\n" );
		break;

	case FC_GCUT_2D:	/* Call a 2d graphical CUT */
		fprintf( fp, "GCUT_2D\n" );
		break;

	default:
		sf_report( "dump_flow: Unkown Fcode ( %d )\n",
			opc & MASK_FC );
		running = FALSE;
		break;
	}

	return running;
}


void
dump_segment(
	FILE *		fp,
	char *		leader,
	QueryExe *	qe,
	Segment		s
)
{
	register FCode		fc;
	bool			running, halt_flag, vec_flag, test_flag, info_flag;
	UInt32			opc;
	Category		cat;

	pc = s;
	pc0 = pc;
	for ( running=TRUE ; running ; ) {
		/* decode instruction */
		opc = *pc;
		pc_rel = pc - pc0;
		fc = opc & MASK_FC;
		pc += 1;
		cat = qp_fc_to_cat[ fc >> CAT_BITS ];

		halt_flag = (opc & FC_HALT_BIT) != 0 ;
		vec_flag = (opc & FC_VECTOR_BIT) != 0 ;
		test_flag = (opc & FC_DYNAMIC_TEST_BIT) != 0 ;
		info_flag = (opc & FC_DYNAMIC_INFO_BIT) != 0 ;

		fprintf( fp, "%-6s%03d ", leader, pc_rel );
		if ( halt_flag || test_flag || info_flag ) {
			fputc( '<', fp );
			if ( halt_flag ) { fputs( "h", fp ); }
			if ( vec_flag ) { fputs( "v", fp ); }
			if ( test_flag ) { fputs( "t", fp ); }
			if ( info_flag ) { fputs( "i", fp ); }
			fputc( '>', fp );
		}
		fputc( '\t', fp );

		switch ( cat ) {

		case CAT_NONE:
			sf_report( "dump_segment: Illegal category CAT_NONE\n");
			running = FALSE;
			break;

		case CAT_OP:
			{
				DataType	dt = fc >> CAT_BITS;

				fprintf( fp, "OPERATOR <%s> %s\n",
					datatype_to_str( dt ), 
					optype_to_str( fc_to_optype( fc ) ) );

				if ( test_flag ) {
					fprintf( fp, "\t\tshape_check=%d\n", *pc++ );
				}
			}
			break;

		case CAT_CONV:
			{
				int		base, from, to;
				static DataType	dt[] = { D_BOOL, D_UINT,
							D_ULONG, D_INT, D_LONG,
							D_FLOAT, D_DOUBLE };

				base = fc - FC_B_2_B;
				to = base % 7;
				base /= 7;
				from = base;
				
				fprintf( fp, "CONV <%s> to <%s>\n",
					datatype_to_str( dt[from] ),
					datatype_to_str( dt[to] ) );
			}
			break;

		case CAT_CONST:
			sf_report( "dump_segment: CAT_CONST not implemented\n");
			running = FALSE;
			break;

		case CAT_MATH_SINGLE:
			running = dump_fun( fp, opc, D_FLOAT );
			break;

		case CAT_MATH_DOUBLE:
			running = dump_fun( fp, opc, D_DOUBLE );
			break;

		case CAT_BITOP:
			running = dump_bitop( fp, opc );
			break;

		case CAT_VAR:
			running = dump_var( fp, opc, qe );
			break;

		case CAT_LITERAL:
			running = dump_literal( fp, opc );
			break;

		case CAT_COMIS:
			running = dump_comis( fp, opc );
			break;

		case CAT_DYN:
			running = dump_dyn( fp, opc, qe );
			break;


		case CAT_MASK:
			running = dump_mask( fp, opc, qe );
			break;


		case CAT_FLOW:
			running = dump_flow( fp, opc );
			break;

		default:
			sf_report( "dump_segment: Unkown category ( %d )\n",
				cat );
			running = FALSE;
			break;
		}

		if ( halt_flag ) {
			running = FALSE;
		}
	}
}
