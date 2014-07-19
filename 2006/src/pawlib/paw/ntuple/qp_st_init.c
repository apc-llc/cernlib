/*
 *  qp_st_init.c  --
 *	Fill the symbol table with the predefined names
 *
 *  Original: 24-Jan-1995 15:47
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_st_init.c,v 1.12 1996/04/23 18:39:02 maartenb Exp $
 *
 *  $Log: qp_st_init.c,v $
 *  Revision 1.12  1996/04/23 18:39:02  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	"errors.h"
#include	"fcode.h"
#include	"hbook_defs.h"
#include	"qp_dimension.h"
#include	"qp_report.h"
#include	"qp_signature.h"
#include	"qp_st_init.h"
#include	"qp_symtab.h"


typedef struct _fill_table_ {
	char		*name;
	FCode		fc;
	Dimension	*ret;
	int		argc;
	Dimension	*argv[MAX_DIMENSION];
} FillTable;


#if 0
/* just for reference ... */
typedef struct _dimension_ {
	DataType	dtyp;		/* The basic type */

	bool		isRange;	/* for integer types */
	int		ilow, iup;

	int		strSize;	/* for strings */

	int		ndim;		/* == 0 for scalars */
	STIndex		idx;		/* last dimension is dynamic */
					/* points to symtab definition */
	int		ranges[MAX_DIMENSION];	/* the range for each */
						/* dimension */
} Dimension;
#endif


static Dimension d_bool =	{ D_BOOL, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_UInt32 =	{ D_UINT, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_UInt64 =	{ D_ULONG, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_Int32 =	{ D_INT, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_Int64 =	{ D_LONG, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_Float32 =	{ D_FLOAT, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_Float64 =	{ D_DOUBLE, FALSE, 0, 0, 0, 0, 0, { 0, } };
static Dimension d_str =	{ D_STR, FALSE, 0, 0, 0, 0, 0, { 0, } };

static Dimension d_u_bool =	{ D_BOOL, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_UInt32 =	{ D_UINT, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_UInt64 =	{ D_ULONG, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_Int32 =	{ D_INT, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_Int64 =	{ D_LONG, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_Float32 =	{ D_FLOAT, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_Float64 =	{ D_DOUBLE, FALSE, 0, 0, 0, -1, 0, { 0, } };
static Dimension d_u_str =	{ D_STR, FALSE, 0, 0, 0, -1, 0, { 0, } };

static FillTable TheFillTable[] = {
	{ "pi", FC_PI, &d_u_Float32, 0, { 0, } },
	{ "dpi", FC_DPI, &d_u_Float64, 0, { 0, } },

	{ "true", FC_TRUE, &d_bool, 0, { 0, } },
	{ "false", FC_FALSE, &d_bool, 0, { 0, } },

	{ "uint32_min", FC_UINT32_MIN, &d_UInt32, 0, { 0, } },
	{ "uint32_max", FC_UINT32_MAX, &d_UInt32, 0, { 0, } },
	{ "uint64_min", FC_UINT64_MIN, &d_UInt64, 0, { 0, } },
	{ "uint64_max", FC_UINT64_MAX, &d_UInt64, 0, { 0, } },

	{ "int32_min", FC_INT32_MIN, &d_Int32, 0, { 0, } },
	{ "int32_max", FC_INT32_MAX, &d_Int32, 0, { 0, } },
	{ "int64_min", FC_INT64_MIN, &d_Int64, 0, { 0, } },
	{ "int64_max", FC_INT64_MAX, &d_Int64, 0, { 0, } },

	{ "float32_min", FC_FLOAT32_MIN, &d_Float32, 0, { 0, } },
	{ "float32_max", FC_FLOAT32_MAX, &d_Float32, 0, { 0, } },
	{ "float64_min", FC_FLOAT64_MIN, &d_Float64, 0, { 0, } },
	{ "float64_max", FC_FLOAT64_MAX, &d_Float64, 0, { 0, } },

/* builtin math functions single precision */

	{ "sin", FC_FSIN, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "cos", FC_FCOS, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "sqrt", FC_FSQRT, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "exp", FC_FEXP, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "log", FC_FLOG, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "atan", FC_FATAN, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "abs", FC_FABS, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "log10", FC_FLOG10, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "tanh", FC_FTANH, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "acos", FC_FACOS, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "asin", FC_FASIN, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "tan", FC_FTAN, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "sinh", FC_FSINH, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "cosh", FC_FCOSH, &d_u_Float32, 1, { &d_u_Float32, } },

	{ "mod", FC_FMOD, &d_u_Float32, 2, { &d_u_Float32, &d_u_Float32, } },
	{ "atan2", FC_FATAN2, &d_u_Float32, 2, { &d_u_Float32, &d_u_Float32, } },
	{ "sign", FC_FSIGN, &d_u_Float32, 2, { &d_u_Float32, &d_u_Float32, } },

	{ "fint", FC_FINT, &d_u_Float32, 1, { &d_u_Float32, } },
	{ "frac", FC_FFRAC, &d_u_Float32, 1, { &d_u_Float32, } },

/* builtin math functions double precision */

	{ "sin", FC_DSIN, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "cos", FC_DCOS, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "sqrt", FC_DSQRT, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "exp", FC_DEXP, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "log", FC_DLOG, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "atan", FC_DATAN, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "abs", FC_DABS, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "log10", FC_DLOG10, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "tanh", FC_DTANH, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "acos", FC_DACOS, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "asin", FC_DASIN, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "tan", FC_DTAN, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "sinh", FC_DSINH, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "cosh", FC_DCOSH, &d_u_Float64, 1, { &d_u_Float64, } },

	{ "mod", FC_DMOD, &d_u_Float64, 2, { &d_u_Float64, &d_u_Float64, } },
	{ "atan2", FC_DATAN2, &d_u_Float64, 2, { &d_u_Float64, &d_u_Float64, } },
	{ "sign", FC_DSIGN, &d_u_Float64, 2, { &d_u_Float64, &d_u_Float64, } },

	{ "fint", FC_DINT, &d_u_Float64, 1, { &d_u_Float64, } },
	{ "frac", FC_DFRAC, &d_u_Float64, 1, { &d_u_Float64, } },

/* builtin math functions int and long int (CAT_BITOP) */

	{ "abs", FC_IABS, &d_u_UInt32, 1, { &d_u_UInt32, } },
	{ "mod", FC_IMOD, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
	{ "sign", FC_ISIGN, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },

	{ "abs", FC_IABS+16, &d_u_UInt64, 1, { &d_u_UInt64, } },
	{ "mod", FC_IMOD+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
	{ "sign", FC_ISIGN+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },

	{ "abs", FC_IABS+32, &d_u_Int32, 1, { &d_u_Int32, } },
	{ "mod", FC_IMOD+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
	{ "sign", FC_ISIGN+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },

	{ "abs", FC_IABS+48, &d_u_Int64, 1, { &d_u_Int64, } },
	{ "mod", FC_IMOD+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
	{ "sign", FC_ISIGN+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },

/* Bit operations according to MIL-STD-1753 */

{ "ior", FC_IOR, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
{ "iand", FC_IAND, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
{ "ieor", FC_IEOR, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
{ "ishft", FC_ISHFT, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_Int32, } },
{ "ishftc", FC_ISHFTC, &d_u_UInt32, 3, { &d_u_UInt32, &d_u_Int32, &d_u_Int32, } },
{ "ibits", FC_IBITS, &d_u_UInt32, 3, { &d_u_UInt32, &d_u_Int32, &d_u_Int32, } },
{ "mvbits", FC_MVBITS, &d_u_UInt32, 5,
	{ &d_u_UInt32, &d_u_Int32, &d_u_Int32, &d_u_UInt32, &d_u_Int32, } },
{ "btest", FC_BTEST, &d_u_bool, 2, { &d_u_UInt32, &d_u_Int32, } },
{ "ibset", FC_IBSET, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_Int32, } },
{ "ibclr", FC_IBCLR, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_Int32, } },

{ "not", FC_INOT, &d_u_UInt32, 1, { &d_u_UInt32, } },


{ "ior", FC_IOR+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
{ "iand", FC_IAND+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
{ "ieor", FC_IEOR+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
{ "ishft", FC_ISHFT+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_Int32, } },
{ "ishftc", FC_ISHFTC+16, &d_u_UInt64, 3, { &d_u_UInt64, &d_u_Int32, &d_u_Int32, } },
{ "ibits", FC_IBITS+16, &d_u_UInt64, 3, { &d_u_UInt64, &d_u_Int32, &d_u_Int32, } },
{ "mvbits", FC_MVBITS+16, &d_u_UInt64, 5,
	{ &d_u_UInt64, &d_u_Int32, &d_u_Int32, &d_u_UInt64, &d_u_Int32, } },
{ "btest", FC_BTEST+16, &d_u_bool, 2, { &d_u_UInt64, &d_u_Int32, } },
{ "ibset", FC_IBSET+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_Int32, } },
{ "ibclr", FC_IBCLR+16, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_Int32, } },

{ "not", FC_INOT+16, &d_u_UInt64, 1, { &d_u_UInt64, } },


{ "ior", FC_IOR+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "iand", FC_IAND+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "ieor", FC_IEOR+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "ishft", FC_ISHFT+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "ishftc", FC_ISHFTC+32, &d_u_Int32, 3, { &d_u_Int32, &d_u_Int32, &d_u_Int32, } },
{ "ibits", FC_IBITS+32, &d_u_Int32, 3, { &d_u_Int32, &d_u_Int32, &d_u_Int32, } },
{ "mvbits", FC_MVBITS+32, &d_u_Int32, 5,
	{ &d_u_Int32, &d_u_Int32, &d_u_Int32, &d_u_Int32, &d_u_Int32, } },
{ "btest", FC_BTEST+32, &d_u_bool, 2, { &d_u_Int32, &d_u_Int32, } },
{ "ibset", FC_IBSET+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "ibclr", FC_IBCLR+32, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },

{ "not", FC_INOT+32, &d_u_Int32, 1, { &d_u_Int32, } },


{ "ior", FC_IOR+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
{ "iand", FC_IAND+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
{ "ieor", FC_IEOR+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
{ "ishft", FC_ISHFT+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int32, } },
{ "ishftc", FC_ISHFTC+48, &d_u_Int64, 3, { &d_u_Int64, &d_u_Int32, &d_u_Int32, } },
{ "ibits", FC_IBITS+48, &d_u_Int64, 3, { &d_u_Int64, &d_u_Int32, &d_u_Int32, } },
{ "mvbits", FC_MVBITS+48, &d_u_Int64, 5,
	{ &d_u_Int64, &d_u_Int32, &d_u_Int32, &d_u_Int64, &d_u_Int32, } },
{ "btest", FC_BTEST+48, &d_u_bool, 2, { &d_u_Int64, &d_u_Int32, } },
{ "ibset", FC_IBSET+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int32, } },
{ "ibclr", FC_IBCLR+48, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int32, } },

{ "not", FC_INOT+48, &d_u_Int64, 1, { &d_u_Int64, } },

/* All the internal and external conversion routines */

#include	"qp_symtab_init_conv.h"

/* Vector reduction routines */

{ "vadd", FC_VADD+D_UINT*64, &d_UInt32, 1, { &d_u_UInt32, } },
{ "vadd", FC_VADD+D_ULONG*64, &d_UInt64, 1, { &d_u_UInt64, } },
{ "vadd", FC_VADD+D_INT*64, &d_Int32, 1, { &d_u_Int32, } },
{ "vadd", FC_VADD+D_LONG*64, &d_Int64, 1, { &d_u_Int64, } },
{ "vadd", FC_VADD+D_FLOAT*64, &d_Float32, 1, { &d_u_Float32, } },
{ "vadd", FC_VADD+D_DOUBLE*64, &d_Float64, 1, { &d_u_Float64, } },

{ "vmul", FC_VMUL+D_UINT*64, &d_UInt32, 1, { &d_u_UInt32, } },
{ "vmul", FC_VMUL+D_ULONG*64, &d_UInt64, 1, { &d_u_UInt64, } },
{ "vmul", FC_VMUL+D_INT*64, &d_Int32, 1, { &d_u_Int32, } },
{ "vmul", FC_VMUL+D_LONG*64, &d_Int64, 1, { &d_u_Int64, } },
{ "vmul", FC_VMUL+D_FLOAT*64, &d_Float32, 1, { &d_u_Float32, } },
{ "vmul", FC_VMUL+D_DOUBLE*64, &d_Float64, 1, { &d_u_Float64, } },

{ "vmin", FC_VMIN+D_UINT*64, &d_UInt32, 1, { &d_u_UInt32, } },
{ "vmin", FC_VMIN+D_ULONG*64, &d_UInt64, 1, { &d_u_UInt64, } },
{ "vmin", FC_VMIN+D_INT*64, &d_Int32, 1, { &d_u_Int32, } },
{ "vmin", FC_VMIN+D_LONG*64, &d_Int64, 1, { &d_u_Int64, } },
{ "vmin", FC_VMIN+D_FLOAT*64, &d_Float32, 1, { &d_u_Float32, } },
{ "vmin", FC_VMIN+D_DOUBLE*64, &d_Float64, 1, { &d_u_Float64, } },

{ "vmax", FC_VMAX+D_UINT*64, &d_UInt32, 1, { &d_u_UInt32, } },
{ "vmax", FC_VMAX+D_ULONG*64, &d_UInt64, 1, { &d_u_UInt64, } },
{ "vmax", FC_VMAX+D_INT*64, &d_Int32, 1, { &d_u_Int32, } },
{ "vmax", FC_VMAX+D_LONG*64, &d_Int64, 1, { &d_u_Int64, } },
{ "vmax", FC_VMAX+D_FLOAT*64, &d_Float32, 1, { &d_u_Float32, } },
{ "vmax", FC_VMAX+D_DOUBLE*64, &d_Float64, 1, { &d_u_Float64, } },

{ "vand", FC_VAND+D_BOOL*64, &d_bool, 1, { &d_u_bool, } },
{ "vor", FC_VOR+D_BOOL*64, &d_bool, 1, { &d_u_bool, } },

/* some extra ``operators'' */

{ "min", FC_MIN+1*64, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
{ "min", FC_MIN+2*64, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
{ "min", FC_MIN+3*64, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "min", FC_MIN+4*64, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
{ "min", FC_MIN+5*64, &d_u_Float32, 2, { &d_u_Float32, &d_u_Float32, } },
{ "min", FC_MIN+6*64, &d_u_Float64, 2, { &d_u_Float64, &d_u_Float64, } },

{ "max", FC_MAX+1*64, &d_u_UInt32, 2, { &d_u_UInt32, &d_u_UInt32, } },
{ "max", FC_MAX+2*64, &d_u_UInt64, 2, { &d_u_UInt64, &d_u_UInt64, } },
{ "max", FC_MAX+3*64, &d_u_Int32, 2, { &d_u_Int32, &d_u_Int32, } },
{ "max", FC_MAX+4*64, &d_u_Int64, 2, { &d_u_Int64, &d_u_Int64, } },
{ "max", FC_MAX+5*64, &d_u_Float32, 2, { &d_u_Float32, &d_u_Float32, } },
{ "max", FC_MAX+6*64, &d_u_Float64, 2, { &d_u_Float64, &d_u_Float64, } },

/* graphical cut pseudo routines */

{ "qp_gcut_1d", FC_GCUT_1D, &d_u_bool, 2, { &d_Int32, &d_u_Float32 } },
{ "qp_gcut_2d", FC_GCUT_2D, &d_u_bool, 3, { &d_Int32, &d_u_Float32, &d_u_Float32, } },

/* string functions */

{ "substr", FC_SUBSTR, &d_u_str, 3, { &d_u_str, &d_u_Int32, &d_u_Int32 } },
{ "match", FC_MATCH, &d_u_bool, 2, { &d_u_str, &d_u_str } },

	{ 0, 0, 0, 0, { 0, } }
};


void
qp_symtab_init( int * errp ) {
	SymTab	*t;
	FillTable	*p;
	Signature	*s;

	qp_assert( sf_symtab == 0 );

	if ( (t = st_new( 500 )) == 0 ) {
		*errp = R_ALLOC_ERROR;
		return ;
	}

	for ( p=TheFillTable; p->name != 0 ; p++ ) {
		s = sig_new_fun( p->fc, p->ret, p->argc, p->argv );
		*errp = st_insert( t, p->name, s );

		if ( *errp != R_NOERR ) {
			break;
		}
	}

	if ( *errp == R_NOERR ) {
		st_mark( t );	/* register current content as permanent */
		sf_symtab = t;
	}
}
