/*
 *  fcode.h  --
 *	The function codes.
 *
 *  Original:  1-Nov-1994 19:05
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: fcode.h,v 1.12 1996/04/23 18:37:56 maartenb Exp $
 *
 *  $Log: fcode.h,v $
 *  Revision 1.12  1996/04/23 18:37:56  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_FCODE
#define CERN_FCODE


#define	CAT_BITS	6
#define MASK_FC		0xfff

/* bit indicating last instruction in segment */
#define	FC_HALT_BIT	0x80000000L

/* bit indicating non scalar operation */
#define	FC_VECTOR_BIT	0x40000000L

/* bit indicating need for test of dynamic size of operands */
#define	FC_DYNAMIC_TEST_BIT	0x20000000L

/* bit indicating need to leave dynamic size of operand(s) on the stack*/
#define	FC_DYNAMIC_INFO_BIT	0x10000000L

/* Define categories of operations */

/*
 * categories are used to split-up the total set of operations 
 * in manageble parts
 */

typedef enum _category_ {
	CAT_NONE = 1,
	CAT_OP,		/* mathematical operators and comparisons */
	CAT_CONV,	/* conversions */
	CAT_CONST,	/* named constants */
	CAT_MATH_SINGLE,/* mathematical function single precision */
	CAT_MATH_DOUBLE,/* mathematical function double precision */
	CAT_BITOP,	/* BITPAK bit operations (MIL-STD-1753)*/
	CAT_VAR,	/* ntuple variables */
	CAT_LITERAL,	/* literals */
	CAT_COMIS,	/* comis calls */
	CAT_DYN,	/* slices of ntup vars */
	CAT_MASK,	/* mask reading and writing */
	CAT_FLOW	/* flow control & debug */
} Category;


typedef enum _fcode_ {


/* numeric operators */
	FC_OR = 0 * 64,	/* reserve 16 slots (max 16 types) */
	FC_AND,
	FC_NOT,

	FC_LT,
	FC_LE,

	FC_LTLT,
	FC_LTLE,
	FC_LELT,
	FC_LELE,

	FC_GT,
	FC_GE,

	FC_GTGT,
	FC_GTGE,
	FC_GEGT,
	FC_GEGE,

	FC_EQ,
	FC_NE,
	FC_CT,

	FC_PLUS,
	FC_MINUS,
	FC_TIMES,
	FC_DIV,

	FC_UMINUS,

	FC_POW,

	FC_VADD,
	FC_VMUL,
	FC_VAND,
	FC_VOR,
	FC_VMIN,
	FC_VMAX,

	FC_MIN,
	FC_MAX,

/* type converters */
	FC_B_2_B = 16 * 64,	/* reserve 4 slots (max 16 types) */
	FC_B_2_U,
	FC_B_2_LU,
	FC_B_2_I,
	FC_B_2_LI,
	FC_B_2_F,
	FC_B_2_LF,
	FC_U_2_B,
	FC_U_2_U,
	FC_U_2_LU,
	FC_U_2_I,
	FC_U_2_LI,
	FC_U_2_F,
	FC_U_2_LF,
	FC_LU_2_B,
	FC_LU_2_U,
	FC_LU_2_LU,
	FC_LU_2_I,
	FC_LU_2_LI,
	FC_LU_2_F,
	FC_LU_2_LF,
	FC_I_2_B,
	FC_I_2_U,
	FC_I_2_LU,
	FC_I_2_I,
	FC_I_2_LI,
	FC_I_2_F,
	FC_I_2_LF,
	FC_LI_2_B,
	FC_LI_2_U,
	FC_LI_2_LU,
	FC_LI_2_I,
	FC_LI_2_LI,
	FC_LI_2_F,
	FC_LI_2_LF,
	FC_F_2_B,
	FC_F_2_U,
	FC_F_2_LU,
	FC_F_2_I,
	FC_F_2_LI,
	FC_F_2_F,
	FC_F_2_LF,
	FC_LF_2_B,
	FC_LF_2_U,
	FC_LF_2_LU,
	FC_LF_2_I,
	FC_LF_2_LI,
	FC_LF_2_F,
	FC_LF_2_LF,

/* constants */
	FC_TRUE = 20 * 64,
	FC_FALSE,
	FC_PI,
	FC_DPI,

	FC_UINT32_MIN,
	FC_UINT32_MAX,
	FC_UINT64_MIN,
	FC_UINT64_MAX,

	FC_INT32_MIN,
	FC_INT32_MAX,
	FC_INT64_MIN,
	FC_INT64_MAX,

	FC_FLOAT32_MIN,
	FC_FLOAT32_MAX,
	FC_FLOAT64_MIN,
	FC_FLOAT64_MAX,

	FC_SUBSTR,
	FC_MATCH,

/* builtin math functions single precision */

	FC_FSIN = 21 * 64,
	FC_FCOS,
	FC_FSQRT,
	FC_FEXP,
	FC_FLOG,
	FC_FATAN,
	FC_FABS,
	FC_FLOG10,
	FC_FTANH,
	FC_FACOS,
	FC_FASIN,
	FC_FTAN,
	FC_FSINH,
	FC_FCOSH,

	FC_FMOD,
	FC_FATAN2,
	FC_FSIGN,

	FC_FINT,
	FC_FFRAC,

	/* ... */
/* builtin math functions double precision */

	FC_DSIN = 22 * 64,
	FC_DCOS,
	FC_DSQRT,
	FC_DEXP,
	FC_DLOG,
	FC_DATAN,
	FC_DABS,
	FC_DLOG10,
	FC_DTANH,
	FC_DACOS,
	FC_DASIN,
	FC_DTAN,
	FC_DSINH,
	FC_DCOSH,

	FC_DMOD,
	FC_DATAN2,
	FC_DSIGN,

	FC_DINT,
	FC_DFRAC,

	/* ... */

/* NTuple var loading */

	FC_CWN_SCA_BOOL = 23 * 64,
	FC_CWN_SCA_UINT,
	FC_CWN_SCA_ULONG,
	FC_CWN_SCA_INT,
	FC_CWN_SCA_LONG,
	FC_CWN_SCA_FLOAT,
	FC_CWN_SCA_DOUBLE,
	FC_CWN_SCA_STR,

	FC_CWN_MAT_BOOL,
	FC_CWN_MAT_UINT,
	FC_CWN_MAT_ULONG,
	FC_CWN_MAT_INT,
	FC_CWN_MAT_LONG,
	FC_CWN_MAT_FLOAT,
	FC_CWN_MAT_DOUBLE,
	FC_CWN_MAT_STR,

	FC_RWN_SCA_LOAD,

/* Literals */

	FC_LIT_SCA_BOOL = 24 * 64,
	FC_LIT_SCA_UINT,
	FC_LIT_SCA_ULONG,
	FC_LIT_SCA_INT,
	FC_LIT_SCA_LONG,
	FC_LIT_SCA_FLOAT,
	FC_LIT_SCA_DOUBLE,
	FC_LIT_SCA_STR,

/* Bit operations */

	/* fc + i * 16 ; i=0,1,2,3 for uint, ulong, int, long */

	FC_IOR = 25 * 64,
	FC_IAND,
	FC_INOT,
	FC_IEOR,
	FC_ISHFT,
	FC_ISHFTC,
	FC_IBITS,
	FC_MVBITS,
	FC_BTEST,
	FC_IBSET,
	FC_IBCLR,
	FC_IABS,
	FC_IMOD,
	FC_ISIGN,

/* Comis calls */

	FC_CS_LFUN = 26 * 64,	/* call comis function returning bool */
	FC_CS_IFUN,	/* call comis function returning integer */
	FC_CS_RFUN,	/* call comis function returning real */
	FC_CS_DFUN,	/* call comis function returning double */
	FC_CS_SFUN,	/* call comis function returning string */

/* slices of ntuple variables */

	/* fc + (ndim -1) * 16 ; ndim 4 and higher are mapped on the same */
	/* code and have and extra word containing the real ndim */

	FC_CWN_DYN_BOOL = 27 * 64,
	FC_CWN_DYN_UINT,
	FC_CWN_DYN_ULONG,
	FC_CWN_DYN_INT,
	FC_CWN_DYN_LONG,
	FC_CWN_DYN_FLOAT,
	FC_CWN_DYN_DOUBLE,
	FC_CWN_DYN_STR,

/* Mask reading and writing */

	FC_MASK_SET_BIT = 28 * 64,	/* set bit */
	FC_MASK_GET_BIT,	/* get bit */
	FC_MASK_GET_WORD,	/* get the whole word */

/* flow control */

	FC_HALT = 31 * 64,
	FC_JUMP,
	FC_POP_JUMP,	/* pop on true, jump on false */
	FC_JUMP_POP,	/* pop on false, jump on true */
	FC_NOP,
	FC_DUMP,
	FC_CHECK_SHAPE,	/* push a static shape on the shape stack */

	FC_CUT,		/* Call an expression CUT */
	FC_GCUT_1D,	/* Call an 1d graphical CUT */
	FC_GCUT_2D	/* Call a 2d graphical CUT */

} FCode;


extern Category		qp_fc_to_cat[];


#endif	/*	CERN_FCODE	*/
