/*
 *  qp_dimension.c  --
 *	Suplly some support for dimension handling
 *	and datatype printing
 *
 *  Original: 24-Nov-1994 15:36
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_dimension.c,v 1.9 1996/04/23 18:38:15 maartenb Exp $
 *
 *  $Log: qp_dimension.c,v $
 *  Revision 1.9  1996/04/23 18:38:15  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<assert.h>
#include	<stdlib.h>

#include	"qp_report.h"
#include	"qp_dimension.h"


static const char * const
_datatype_to_str[D_LAST] = {
	"bool",			/* the order of the numeric types */
	"uint",			/* should not be changed !	  */
	"ulong",		/* see type promotion etc.	  */
	"int",
	"long",
	"float",
	"double",
	"str",
	"undef"
};


int const	datatype_size[] = {
	1,	/* D_BOOL */
	1,	/* D_UINT */
	2,	/* D_ULONG */
	1,	/* D_INT */
	2,	/* D_LONG */
	1,	/* D_FLOAT */
	2,	/* D_DOUBLE */
	8,	/* D_STR */
	-1,	/* D_UNDEF */
};

long const	datatype_align[] = {	/* keep in sync with MAX_ALIGN */
	~0,	/* D_BOOL */
	~0,	/* D_UINT */
	~1,	/* D_ULONG */
	~0,	/* D_INT */
	~1,	/* D_LONG */
	~0,	/* D_FLOAT */
	~1,	/* D_DOUBLE */
	~0,	/* D_STR */
	~0,	/* D_UNDEF */
};


DataType const	datatype_signed[] = {
	D_BOOL,	/* D_BOOL */
	D_INT,	/* D_UINT */
	D_LONG,	/* D_ULONG */
	D_INT,	/* D_INT */
	D_LONG,	/* D_LONG */
	D_FLOAT,/* D_FLOAT */
	D_DOUBLE,/* D_DOUBLE */
	D_STR,	/* D_STR */
	D_UNDEF,/* D_UNDEF */
};


static Dimension *
dim_alloc()
{
	Dimension	*d;

	d = (Dimension *) calloc( sizeof(Dimension), 1 );

	d->idx = STI_INVALID;

	if ( d == 0 ) {
		qp_abort( "Cannot allocate memory for 'Dimension' struct.\n" );
	}

	return d;
}


Dimension *
dim_new( DataType dtyp )
{
	Dimension	*d;

	d = dim_alloc();

	d->dtyp = dtyp;

	return d;
}


Dimension *
dim_copy( Dimension * d )
{
	Dimension	*new_d;

	new_d = dim_alloc();

	*new_d = *d;

	return new_d;
}


/*
 * Create a new dimension structure which has the same shape
 * as the input dimensions. 
 *
 * 1) ndim should be equal, or zero
 *
 * 2) for each dimension the lenghts should be equal or
 *    runtime determined (-1)
 *
 * 3) the maximum size is constructed by taking the minimal
 *    maximum for each dimension
 */
Dimension *
dim_combine3(
	DataType		dtyp,
	Dimension *		d1,
	Dimension *		d2,
	Dimension *		d3
) {
	Dimension	*new_d;
	int		i;

	new_d = dim_alloc();

	/* initialize the base type */
	if ( dtyp != D_UNDEF ) {
		new_d->dtyp = dtyp;
	} else {
		new_d->dtyp = d1->dtyp;
		new_d->isRange = d1->isRange;
		new_d->ilow = d1->ilow;
		new_d->iup = d1->iup;
		new_d->strSize = d1->strSize;
	}
	new_d->idx = STI_INVALID;

	for( i=0; i < MAX_DIMENSION ; i++ ) new_d->range[i] = -1;

	if ( d1->ndim != 0 ) {
		new_d->ndim = d1->ndim;
		for ( i=0; i < new_d->ndim ; i++ ) {
			new_d->range[i] = d1->range[i];
		}
	}

	if ( d2->ndim != 0 ) {
qp_assert( new_d->ndim == d2->ndim || new_d->ndim == 0 );
		new_d->ndim = d2->ndim;
		for ( i=0; i < new_d->ndim ; i++ ) {
			if ( d2->range[i] != -1 ) {
qp_assert( new_d->range[i] == d2->range[i] || new_d->range[i] == -1 );
				new_d->range[i] = d2->range[i];
			}
		}
	}

	if ( d3->ndim != 0 ) {
qp_assert( new_d->ndim == d3->ndim || new_d->ndim == 0 );
		new_d->ndim = d3->ndim;
		for ( i=0; i < new_d->ndim ; i++ ) {
			if ( d3->range[i] != -1 ) {
qp_assert( new_d->range[i] == d3->range[i] || new_d->range[i] == -1 );
				new_d->range[i] = d3->range[i];
			}
		}
	}


	/* calculate the new max_range[] */

	if ( d1->ndim != 0 ) {
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			new_d->max_range[i] = d1->max_range[i];
		}
	}

	if ( d2->ndim != 0 ) {
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			if ( (d1->ndim == 0) ||
				(new_d->max_range[i] > d2->max_range[i]) ) {

				new_d->max_range[i] = d2->max_range[i];
			}
		}
	}

	if ( d3->ndim != 0 ) {
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			if ( (d1->ndim == 0 && d2->ndim == 0) ||
				(new_d->max_range[i] > d3->max_range[i]) ) {

				new_d->max_range[i] = d3->max_range[i];
			}
		}
	}

	return new_d;
}


Dimension *
dim_combine( DataType dtyp, Dimension * d1, Dimension * d2 )
{
	Dimension	*new_d;
	int		i;

	new_d = dim_alloc();

	/* initialize the base type */
	if ( dtyp != D_UNDEF ) {
		new_d->dtyp = dtyp;
	} else {
		new_d->dtyp = d1->dtyp;
		new_d->isRange = d1->isRange;
		new_d->ilow = d1->ilow;
		new_d->iup = d1->iup;
		new_d->strSize = d1->strSize;
	}
	new_d->idx = STI_INVALID;

	/*
	 * Initialize the shape, d1 & d2 are equal, one is scalar or
	 * dimensions might be scalar in either or both
	 */

	if ( d1->ndim == 0 ) {
		new_d->ndim = d2->ndim;
		for ( i=0; i < new_d->ndim ; i++ ) {
			new_d->range[i] = d2->range[i];
		}
	} else if ( d2->ndim == 0 ) {
		new_d->ndim = d1->ndim;
		for ( i=0; i < new_d->ndim ; i++ ) {
			new_d->range[i] = d1->range[i];
		}
	} else {	/* d1->ndim == d2->ndim */
		new_d->ndim = d1->ndim;
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			if ( d1->range[i] == -1 ) {
				new_d->range[i] = d2->range[i];
			} else {
				new_d->range[i] = d1->range[i];
			}
		}
	}

	/* calculate the new max_range[] */

	if ( d1->ndim == 0 ) {
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			new_d->max_range[i] = d2->max_range[i];
		}
	} else if ( d2->ndim == 0 ) {
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			new_d->max_range[i] = d1->max_range[i];
		}
	} else {	/* d1->ndim == d2->ndim */
		for ( i=0 ; i < new_d->ndim ; i++ ) {
			if ( d1->max_range[i] < d2->max_range[i] ) {
				new_d->max_range[i] = d1->max_range[i];
			} else {
				new_d->max_range[i] = d2->max_range[i];
			}
		}
	}

	return new_d;
}


void
dim_del( Dimension * d )
{
	free( d );
}


bool
dim_static( Dimension *d )
{
	int	i;

	if ( d->ndim == -1 ) {
		return FALSE;
	}

	for ( i=0 ; i < d->ndim ; i++ ) {
		if ( d->range[i] == -1 ) {
			return FALSE;
		}
	}

	return TRUE;
}


int
dim_maxsize( Dimension *d )
{
	int	i, size;

	assert( d->ndim != -1 );

	size = 1;
	for ( i=0 ; i < d->ndim ; i++ ) {
		size *= d->max_range[i];
	}

	return size;
}


void
dump_dimension( FILE * const fp, Dimension *d ) {
	int	i;

	fprintf( fp, "{ %s", _datatype_to_str[d->dtyp] );
	if ( d->dtyp == D_STR ) {
		fprintf( fp, "*%d", d->strSize );
	}
	if ( d->isRange ) {
		fprintf( fp, "[%d,%d]", d->ilow, d->iup );
	}
	if ( d->ndim != 0 ) {
		fputs( " (", fp );
		if ( d->ndim == -1 ) {
			fputc( '*', fp );
		} else {
			for( i=0; i < d->ndim ; i++ ) {
				if (i > 0 )
					fputc( ',', fp );
				fprintf( fp, "%d", d->range[i] );
			}
			fputs( ") max(", fp );
			for( i=0; i < d->ndim ; i++ ) {
				if (i > 0 )
					fputc( ',', fp );
				fprintf( fp, "%d", d->max_range[i] );
			}
		}
		fputc( ')', fp );
	}
	fputs( " }", fp );
}


void
print_dimension( FILE * const fp, Dimension *d ) {
	int	i;

	fputs( _datatype_to_str[d->dtyp], fp );
	if ( d->dtyp == D_STR ) {
		fprintf( fp, "*%d", d->strSize );
	}
	if ( d->isRange ) {
		fprintf( fp, "[%d,%d]", d->ilow, d->iup );
	}
	if ( d->ndim != 0 ) {
		fputs( "(", fp );
		if ( d->ndim == -1 ) {
			fputc( '*', fp );
		} else {
			for( i=0; i < d->ndim ; i++ ) {
				if (i > 0 )
					fputc( ',', fp );
				if( d->range[i] == -1 ) {
					fputc( '*', fp );
				} else {
					fprintf( fp, "%d", d->range[i] );
				}
			}
		}
		fputc( ')', fp );
	}
}


const char *
datatype_to_str( DataType dtyp )
{
	return _datatype_to_str[ dtyp ];
}
