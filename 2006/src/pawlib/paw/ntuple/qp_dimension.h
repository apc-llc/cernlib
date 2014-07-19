/*
 *  qp_dimension.h  --
 *	The static type description
 *
 *  Original:  1-Nov-1994 15:46
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_dimension.h,v 1.7 1996/04/23 18:38:15 maartenb Exp $
 *
 *  $Log: qp_dimension.h,v $
 *  Revision 1.7  1996/04/23 18:38:15  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_DIMENSION
#define CERN_DIMENSION

#include	<stdio.h>


#include	"cern_types.h"
#include	"hbook_defs.h"


typedef int	STIndex;
#define STI_INVALID	((STIndex) -1)

typedef enum _data_type_ {
	D_BOOL,			/* the order of the numeric types */
	D_UINT,			/* should not be changed !	  */
	D_ULONG,		/* see type promotion, fcode, etc.*/
	D_INT,
	D_LONG,
	D_FLOAT,
	D_DOUBLE,
	D_STR,
	D_UNDEF,
	D_LAST
} DataType;


#define	D_BOOL_MIN	FALSE
#define	D_UINT_MIN	0
#define	D_ULONG_MIN	0
#define	D_INT_MIN	INT32_MIN
#define	D_LONG_MIN	INT64_MIN
#define	D_FLOAT_MIN	(-FLOAT32_MAX)
#define	D_DOUBLE_MIN	(-FLOAT64_MAX)

#define	D_BOOL_MAX	TRUE
#define	D_UINT_MAX	UINT32_MAX
#define	D_ULONG_MAX	UINT64_MAX
#define	D_INT_MAX	INT32_MAX
#define	D_LONG_MAX	INT64_MAX
#define	D_FLOAT_MAX	FLOAT32_MAX
#define	D_DOUBLE_MAX	FLOAT64_MAX


typedef struct _dimension_ {
	DataType	dtyp;		/* The basic type */

	bool		isRange;	/* for integer types */
	int		ilow, iup;

	int		strSize;	/* for strings */

	int		ndim;		/* == 0 for scalars */
	STIndex		idx;		/* last dimension is dynamic */
					/* points to symtab definition */
	int		range[MAX_DIMENSION];	/* the range for each */
						/* dimension */
	int		max_range[MAX_DIMENSION]; /* the maximum value a */
							/* range can have */
} Dimension;


#define	MAX_ALIGN	(~1)

extern long const	datatype_align[];
extern int const	datatype_size[];
extern DataType const	datatype_signed[];


Dimension *
dim_new( DataType dtyp );

Dimension *
dim_copy( Dimension * d );

Dimension *
dim_combine( DataType dtyp, Dimension * d1, Dimension * d2 );

Dimension *
dim_combine3(
	DataType		dtyp,
	Dimension *		d1,
	Dimension *		d2,
	Dimension *		d3
);

void
dim_del( Dimension * d );

bool
dim_static( Dimension * d );

int
dim_maxsize( Dimension * d );

void
dump_dimension( FILE * const fp, Dimension * d );

void
print_dimension( FILE * const fp, Dimension * d );

const char *
datatype_to_str( DataType dtyp );


#endif	/*	CERN_DIMENSION	*/
