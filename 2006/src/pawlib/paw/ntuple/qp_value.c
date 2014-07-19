/*
 *  qp_value.c  --
 *	implement storage and operations on 
 *	constant values.
 *
 *  Original:  1-Nov-1994 15:37
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_value.c,v 1.12 1996/05/14 12:23:34 maartenb Exp $
 *
 *  $Log: qp_value.c,v $
 *  Revision 1.12  1996/05/14 12:23:34  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.11  1996/04/23 18:39:07  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<math.h>
#include	<stdlib.h>
#include	<string.h>

#include	"cern_types.h"
#include	"errors.h"
#include	"qpflags.h"
#include	"qp_report.h"
#include	"qp_value.h"
#include	"str.h"


static void
spaces( FILE * fp, int n ){
	register int i;

	for( i=4*n; i > 0 ; i-- )
		fputc( ' ', fp );
}


void
val_print( FILE * fp, Value * v ) {

	if ( v->d->ndim == 0 ) {
		switch( v->d->dtyp ) {	/* switch DataType */
		case D_UNDEF:
				fprintf( fp, "D_UNDEF" );
				break;
		case D_BOOL:
				fprintf( fp, "%s", v->u.bval ? ".true." : ".false." );
				break;
		case D_UINT:
				fprintf( fp, "z'%x'", v->u.uval );
				break;
		case D_ULONG:
				fprintf( fp, "z'%lx'", v->u.luval );
				break;
		case D_INT:
				fprintf( fp, "%d", v->u.ival );
				break;
		case D_LONG:
				fprintf( fp, "%ld", v->u.lival );
				break;
		case D_FLOAT:
				{ char	b[32];
					sprintf( b, "%.17lg", v->u.fval );
					if ( ! strchr( b, '.' ) ) strcat( b, "." );
					fputs( b, fp );
				}
				break;
		case D_DOUBLE:
				{ char	b[32];
					sprintf( b, "%.17lg", v->u.lfval );
					if ( ! strchr( b, '.' ) ) strcat( b, "." );
					fputs( b, fp );
				}
				break;
		case D_STR:
				fprintf( fp, "'%s'", v->u.sval );
				break;
		default:
				qp_abort( "val_print: Unknown DataType %d\n", v->d->dtyp );
				break;
		}
	} else {
		fprintf( fp, "<ARRAY>" );
	}
}


void
val_dump( FILE * fp, int lvl, Value * v ) {
	spaces( fp, lvl );

	if ( v->d->ndim == 0 ) {
		fputs( " = { ", fp );
		switch( v->d->dtyp ) {	/* switch DataType */
		case D_UNDEF:
				fprintf( fp, "D_UNDEF" );
				break;
		case D_BOOL:
				fprintf( fp, "%s", v->u.bval ? ".true." : ".false." );
				break;
		case D_UINT:
				fprintf( fp, "z'%x'", v->u.uval );
				break;
		case D_ULONG:
				fprintf( fp, "z'%lx'", v->u.luval );
				break;
		case D_INT:
				fprintf( fp, "%d", v->u.ival );
				break;
		case D_LONG:
				fprintf( fp, "%ld", v->u.lival );
				break;
		case D_FLOAT:
				{ char	b[32];
					sprintf( b, "%.17lg", v->u.fval );
					if ( ! strchr( b, '.' ) ) strcat( b, "." );
					fputs( b, fp );
				}
				break;
		case D_DOUBLE:
				{ char	b[32];
					sprintf( b, "%.17lg", v->u.lfval );
					if ( ! strchr( b, '.' ) ) strcat( b, "." );
					fputs( b, fp );
				}
				break;
		case D_STR:
				fprintf( fp, "'%s'", v->u.sval );
				break;
		default:
				qp_abort( "val_dump: Unknown DataType %d\n", v->d->dtyp );
				break;
		}
		fputs( " }", fp );
	} else {
		fprintf( fp, "<ARRAY>" );
	}
}


static Value *
val_alloc()
{
	Value	*v;

	v = (Value *) calloc( sizeof(Value), 1 );

	if ( v == 0 ) {
		qp_abort( "Cannot allocate memory for 'Value' struct.\n" );
	}

	return v;
}


static void *
val_alloc_buf( void * data, int len )
{
	void	*p;

	p = malloc( len );

	if ( p == 0 ) {
		qp_abort( "Cannot allocate memory for 'Value' buffer.\n" );
	}

	(void) memcpy( p, data, len );

	return p;
}


Value *
val_eval( FCode fc_i, int valc, Value *valv[MAX_DIMENSION],  int * err )
{
	FCode	fc = fc_i;
	Value	*r;
	int	i;

	if ( qp_flags_get( "stat_eval" ) == 0 ) {
		return 0;
	}

	for( i=0 ; i < valc ; i++ ) {
		if ( valv[i]->d->ndim !=0 ) {
			sf_report( "val_eval: Warning Multi-dim eval not implemented" );
			return 0;
		}
	}

	r = val_alloc();

/*
 *  OP_FIELD	the name of the field in the Value structure union
 *  OP_TYPE	the type for declarations and casts
 *  OP_DTYPE	The Datatype value for this type
 */

	if ( valc == 0 ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#include	"qp_eval_fun_const.h"
#include	"qp_eval_conv.h"

		}
	} else if ( valv[0]->d->dtyp == D_BOOL ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#include	"qp_eval_op_bool.h"

		}
	} else if ( valv[0]->d->dtyp == D_UINT ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		UInt32
#define	OP_FIELD	uval
#define	OP_DTYPE	D_UINT
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_op_cmp.h"
#include	"qp_eval_fun_bitop.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_ULONG ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		UInt64
#define	OP_FIELD	luval
#define	OP_DTYPE	D_ULONG
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_op_cmp.h"
#include	"qp_eval_fun_bitop.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_INT ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		Int32
#define	OP_FIELD	ival
#define	OP_DTYPE	D_INT
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_op_cmp.h"
#include	"qp_eval_fun_bitop.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_LONG ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		Int64
#define	OP_FIELD	lival
#define	OP_DTYPE	D_LONG
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_op_cmp.h"
#include	"qp_eval_fun_bitop.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_FLOAT ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		Float32
#define	OP_FIELD	fval
#define	OP_DTYPE	D_FLOAT
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_fun_single_math.h"
#include	"qp_eval_op_cmp.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_DOUBLE ) {
		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		Float64
#define	OP_FIELD	lfval
#define	OP_DTYPE	D_DOUBLE
#include	"qp_eval_op_num.h"
#include	"qp_eval_conv.h"
#include	"qp_eval_fun_double_math.h"
#include	"qp_eval_op_cmp.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else if ( valv[0]->d->dtyp == D_STR ) {

		switch( fc ) {	/* switch FCode */
			case FC_NOP: break;
			default:
				sf_report( "val_eval: Warning FCode %d not implemented\n",
						fc );
				free( r ); r = 0;
				break;

#define	OP_TYPE		String
#define	OP_FIELD	sval
#define	OP_DTYPE	D_STRING
#include	"qp_eval_op_cmp_str.h"
#undef	OP_DTYPE
#undef	OP_FIELD
#undef	OP_TYPE
		}
	} else {
		sf_report( "val_eval: Unknown DataType %d\n", valv[0]->d->dtyp );
		*err = R_INTERNAL_ERROR;
		return 0;
	}

	return r;
}


Value *
val_new_bool( bool bval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_BOOL );
	v->u.bval = bval;

	return v;
}


Value *
val_new_uint( unsigned int uval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_UINT );
	v->u.uval = uval;

	return v;
}


Value *
val_new_ulong( UInt64 luval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_ULONG );
	v->u.uval = luval;

	return v;
}


Value *
val_new_int( int ival )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_INT );
	v->u.ival = ival;

	return v;
}


Value *
val_new_float( double lfval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_FLOAT );
	if ( lfval > FLOAT32_MAX ) {
		v->u.fval = FLOAT32_MAX;
	} else if ( lfval < 0 && -FLOAT32_MAX ) {
		v->u.fval = -FLOAT32_MAX;
	} else {
		v->u.fval = lfval;
	}

	return v;
}


Value *
val_new_double( double lfval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_DOUBLE );
	v->u.lfval = lfval;

	return v;
}


Value *
val_new_vfloat( Dimension *d, float * vfval )
{
	Value	*v;
	int	len, i;

	for( len=1, i=0; i < d->ndim ; i++ )
		len *= d->range[i];

	v = val_alloc();

	v->d = dim_copy( d );
	v->u.vfval = (float *) val_alloc_buf( vfval, sizeof(float) * len );

	return v;
}


Value *
val_new_vint( Dimension *d, int * vival )
{
	Value	*v;
	int	len, i;

	for( len=1, i=0; i < d->ndim ; i++ )
		len *= d->range[i];

	v = (Value *) calloc( sizeof(Value), 1 );

	v->d = dim_copy( d );
	v->u.vival = (int *) val_alloc_buf( vival, sizeof(float) * len );

	return v;
}


Value *
val_new_str( char * sval )
{
	Value	*v;

	v = val_alloc();

	v->d = dim_new( D_STR );
	v->u.sval = str_new(sval);

	return v;
}


Value *
val_copy( Value * v )
{
	int		n, i;
	Value *		v_new;

	v_new = val_alloc();

	v_new->d = dim_copy( v->d );

	switch( v->d->dtyp ) {	/* switch DataType */
	case D_BOOL:
	case D_UINT:
	case D_INT:
	case D_FLOAT:
	case D_DOUBLE:
				
		if ( v->d->ndim == 0 ) {
			v_new->u = v->u;
		} else {
			for ( i=0, n=1; i < v->d->ndim ; i++ )
				n *= v->d->range[i];
			n *= datatype_size[v->d->dtyp] * 4;
			v_new->u.vbval = val_alloc_buf( v->u.vbval, n );
		}
		break;

	case D_STR:

		if ( v->d->ndim == 0 ) {
			v_new->u.sval = str_new( v->u.sval );
		} else {
			for ( i=0, n=1; i < v->d->ndim ; i++ )
				n *= v->d->range[i];
			for ( i=0; i < n ; i++ )
				v_new->u.vsval[i] = str_new( v->u.vsval[i] );
		}
		break;

	default:

		qp_abort(
			"Internal error. DataType %d unknown\n",
			v->d->dtyp );
		break;
	}

	return v_new;
}


void
val_del( Value * v )
{
	int	i, n;

	if ( v->d->ndim != 0 ) {
		switch( v->d->dtyp ) {	/* switch DataType */
		case D_BOOL:
				free( v->u.vbval );
				break;
		case D_UINT:
				free( v->u.vuval );
				break;
		case D_INT:
				free( v->u.vival );
				break;
		case D_FLOAT:
		case D_DOUBLE:
				free( v->u.vfval );
				break;
		case D_STR:
				for ( i=0, n=1; i < v->d->ndim ; i++ )
					n *= v->d->range[i];

				for ( i=0; i < n ; )
					free( v->u.vsval[i] );

				free( v->u.vsval );

				break;
		default:
				qp_abort(
					"Internal error. DataType %d unknown\n",
					v->d->dtyp );
				break;
		}
	} else if ( v->d->dtyp == D_STR ) {
		free( v->u.sval );
	}

	dim_del( v->d );

	free( v );
}


Value *
val_comis( Signature *s, int valc, Value *valv[MAX_DIMENSION],  int * err )
{
	return 0;
}
