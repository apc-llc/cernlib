/*
 *  qp_cuts.c  --
 *	Implement the paw ntuple/cuts command and the associated
 *	store of cut definitions. Both normal and graphical cuts
 *	are handled.
 *
 *  Original: 23-Sep-1994 17:36
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_cuts.c,v 1.21 1999/07/01 14:18:44 couet Exp $
 *
 *  $Log: qp_cuts.c,v $
 *  Revision 1.21  1999/07/01 14:18:44  couet
 *  - higz_interface.h in now replaced by higz.h in CVSCOSRC
 *
 *  Revision 1.20  1999/06/28 15:08:55  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.19  1997/04/08 12:35:00  couet
 *  - $CUT on undefined cut was not working:
 *
 *  PAW > mess $cut($10)
 *  /KUIP/LAST: Assertion failed: cut_list[ cid ] != 0, file /afs/.cern.ch/asis/share/cern/97a/src/pawlib/paw/ntuple/qp_cuts.c, line 104
 *  Abort
 *
 *  Revision 1.18  1996/12/05 10:04:22  lecointe
 *  Two completely different things
 *  	. add a _C to "paqcut" in "qp_cuts.c" to avoid duplicate symbol
 *  on VMS
 *  	. Modified "expand_var_list" called by "do_nt_scan" in npantup.c.
 *  Removed the hand-made parser and use "ku_getl" to parse the list of
 *  variables to scan.
 *
 *  Revision 1.17  1996/09/03 11:51:17  lecointe
 *  Added "paqcut" with a cfortran wrapper, to be called from KUIP.
 *  Needed to restore $CUT and $CUTEXPAND as KUIP functions
 *
 *  Revision 1.16  1996/05/15 14:50:11  maartenb
 *  - Fix realloc() of NULL error.
 *
 *  Revision 1.15  1996/04/23 18:38:13  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>


#include	"str.h"

#include	<cfortran/cfortran.h>


#include	"cern_types.h"
#include	"errors.h"
#include	<cfortran/higz.h>
#include	"kuip_interface.h"
#include	"paw_interface.h"
#include	"qp_cuts.h"
#include	"qp_report.h"


static Cut	*cut_list[MAX_CUT_ID+1];


static void cut_add( int cid, char * const expr );

void
gcut_add_1d( int cid, const char * const expr, float xlow, float xhigh );

void
gcut_add_2d( int cid, const char * const expr_x, const char * const expr_y,
	int n, float xv[], float yv[] );


CutType
qp_cut_type( int cid )
{
	qp_assert( 0 < cid && cid <= MAX_CUT_ID );

	if ( cut_list[ cid ] != 0 ) {
		return cut_list[ cid ]->ctyp;
	} else {
		return C_NONE;
	}
}


char *
qp_cut_expr( int cid )
{
	Cut	*p;

	qp_assert( cut_list[ cid ] != 0 );

	p = cut_list[ cid ];

	if ( p->ctyp == C_EXPR ) {
		return p->u.expr.expr;
	}

	return "is_a_graphical_cut";
}


/* For KUIP compatibility */
void
paqcut_C( char *cut_expr, int cid )
{
	Cut	*p;

	if ( cut_list[ cid ] != 0 ) {
	   p = cut_list[ cid ];

	   if ( p->ctyp == C_EXPR ) {
	      strcpy( cut_expr, p->u.expr.expr);
	   } else {
	      strcpy(cut_expr, "is_a_graphical_cut");
	   }
	} else {
	   strcpy(cut_expr, "??");
	}
}
FCALLSCSUB2(paqcut_C, PAQCUT, paqcut, PSTRING, INT)

void
qp_gcut_expr( int cid, char **x, char **y )
{
	Cut	*p;

	qp_assert( cut_list[ cid ] != 0 );

	p = cut_list[ cid ];

	qp_assert( p->ctyp == C_GRAF );

	*x = p->u.graf.exprx;
	*y = p->u.graf.expry;
}


CutGraf *
gcut_get_data( const int cid )
{
	Cut	*p;

	qp_assert( (cid > 0) && (cid <= MAX_CUT_ID) && (cut_list[ cid ] != 0) );

	p = cut_list[ cid ];

	return &p->u.graf;
}


static void
cut_del( int cid )
{
	int		i;
	Cut		*p;

	if ( cid != 0 && cut_list[ cid ] == 0 ) {
		printf( "$%d does not exist\n", cid );
		return;
	}

	for ( i=1 ; i <= MAX_CUT_ID ; i++ ) {
		if ( i == cid || ( cid==0 && cut_list[i] != 0) ) {

			p = cut_list[ i ];

			if ( p->ctyp == C_EXPR ) {
				free( p->u.expr.expr );
			} else if ( p->ctyp == C_GRAF ) {
				free( p->u.graf.exprx );
				free( p->u.graf.expry );
				free( p->u.graf.points );
			}

			if ( p->dep ) {
				free( p->dep );
			}

			free( p );

			cut_list[ i ] = 0;

			if ( cid != 0 ) {
				break;
			}
		}
	}
}


static void
cut_new( int id, int *errp )
{
	Cut	*p;

	if ( cut_list[ id ] != 0 ) {
		sf_report( "Replacing cut $%d\n", id );
		cut_del( id );
	}

	p = (Cut *) calloc( sizeof( Cut ), 1 );
	if ( p == 0 ) {
		sf_report( "cut_new: Allocation error\n" );
		*errp = R_ALLOC_ERROR;
		return;
	}

	p->id = id;
	cut_list[ id ] = p;
}


static void cut_print( int cid );


static void
cut_print_all()
{
	int	i;

	for( i=1 ; i <= MAX_CUT_ID ; i++ ) {
		if ( cut_list[ i ] != 0 )
			cut_print( i );
	}
}


static void
cut_print( int cid )
{
	Cut	*p;
	int	i;

	if ( cid == 0 ) {
		cut_print_all();
		return;
	}

	p = cut_list[ cid ];

	if ( p == 0 ) {
		printf( "$%d does not exist\n", cid );
	} else if ( p->ctyp == C_EXPR ) {
		printf( "$%d  =  %s\n", cid, p->u.expr.expr );
	} else if ( p->ctyp == C_GRAF ) {
		if ( p->u.graf.dim == 1 ) {
			printf( "$%d (graphical cut): %g <= %s < %g\n", cid,
				p->u.graf.points[0][0], p->u.graf.exprx,
				p->u.graf.points[1][0] );
		} else {
			printf( "$%d (graphical cut):\n  X = %s\n  Y = %s\n",
				cid, p->u.graf.exprx, p->u.graf.expry );
			for ( i=0 ; i < p->u.graf.np ; i++ ) {
				if ( (i % 2) == 0 )
					putchar( ' ' );
				putchar( ' ' );
				printf( "( %g, %g )",
					p->u.graf.points[i][0],
					p->u.graf.points[i][1] );
				if ( (i % 2) == 1 )
					putchar( '\n' );
			}
			if ( (i % 2) != 0 )
				putchar( '\n' );
		}
	}

	if ( (p->dep != 0) && (p->num > 0) ) {
		printf( "\t$%d depends on", cid );

		for ( i=0 ; i < p->num ; i++ ) {
			if ( i != 0 ) putchar( ',' );
			printf( " $%d", p->dep[i] );
		}
		putchar( '\n' );
	}
}


static void
cut_read( int cid, char * const filename )
{
	Cut		*p;
	FILE		*fp;
	int		id, dim, np, i;
	CutType		ctyp;
	char		buf[256]; /* to be replaced by a reasonable getline */
	String		exprx, expry;
	float		low, high, *xv, *yv;

	fp = fopen( filename, "r" );
	if ( fp == 0 ) {
		sf_report( "Cannot open %s for reading (%s)\n", filename,
			strerror( errno ) );
		return;
	}

	if ( (fgets( buf, sizeof(buf), fp ) == NULL) 
		|| strcmp( buf, "# Paw Cut File V1.0\n" ) != 0 ) {
		sf_report( "Format error in cut file\n" );
		return;
	}

	while ( fgets( buf, sizeof(buf), fp ) != NULL ) {
		if ( strncmp( buf, "CUT ", 4 ) != 0 ) {
			sf_report( "Format error in cut file\n" ); break;
		}
		id = atoi( &buf[4] );
		if ( id == 0 ) {
			sf_report( "Format error in cut file\n" ); break;
		}

		if ( buf[8] == 'E' ) {
			ctyp = C_EXPR;
		} else {
			ctyp = C_GRAF;
		}

		if ( fgets( buf, sizeof(buf), fp ) == NULL ) {
			sf_report( "Format error in cut file\n" );break;
		}

		if ( ctyp == C_EXPR ) {
			if ( cid == 0 || cid == id ) {
				buf[strlen(buf)-1] = '\0';
				cut_add( id, str_new( buf ) );
			}
		} else {
			dim = atoi( buf );

			if ( fgets( buf, sizeof(buf), fp ) == NULL ) {
				sf_report( "Format error in cut file\n" );break;
			}

			buf[strlen(buf)-1] = '\0';
			exprx = str_new( buf );

			if ( fgets( buf, sizeof(buf), fp ) == NULL ) {
				sf_report( "Format error in cut file\n" );break;
			}

			if ( dim == 1 ) {
				if ( sscanf(buf, "%g %g", &low, &high) != 2 ) {
					sf_report("Format error in cut file\n");
					str_del( exprx );
					break;
				}
				if ( cid == 0 || cid == id ) {
					gcut_add_1d( id, exprx, low, high );
				} else {
					str_del( exprx );
				}
			} else {
				buf[strlen(buf)-1] = '\0';
				expry = str_new( buf );
				if ( fgets( buf, sizeof(buf), fp ) == NULL ) {
					sf_report( "Format error in cut file\n"
						);break;
				}
				np = atoi( buf );

				xv = (float *) calloc( sizeof(float), np );
				yv = (float *) calloc( sizeof(float), np );
				qp_assert( xv != 0 && yv != 0 );

				for ( i=0 ; i < np ; i++ ) {
					if (fgets(buf,sizeof(buf),fp)==NULL) {
						sf_report( "Format error in "
							"cut file\n");break;
					}
					if (sscanf(buf,"%g %g",&low,&high)!=2) {
						sf_report( "Format error in "
							"cut file\n");break;
					}
					xv[i] = low ; yv[i] = high;
				}

				if ( i == np ) {
					if ( cid == 0 || cid == id ) {
						gcut_add_2d( id, exprx, expry,
							np, xv, yv );
					} else {
						str_del( exprx );
						str_del( expry );
					}
				} else {
					sf_report("Format error in cut file\n");
					str_del( exprx );
					str_del( expry );
				}

				free( (void *) xv );
				free( (void *) yv );
			}
		}
	}

	fclose( fp );
}


static void
cut_write_one( FILE *fp, Cut *cp )
{
	int		i;

	fprintf( fp, "CUT %3d %s\n", cp->id, cp->ctyp == C_EXPR ?
		"EXPR" : "GRAF" );
	
	if (  cp->ctyp == C_EXPR ) {
		fprintf( fp, "%s\n", cp->u.expr.expr );
	} else {
		fprintf( fp, "%d\n", cp->u.graf.dim );

		fprintf( fp, "%s\n", cp->u.graf.exprx );
		if ( cp->u.graf.dim == 2 ) {
			fprintf( fp, "%s\n", cp->u.graf.expry );
		}
		if ( cp->u.graf.dim == 1 ) {
			fprintf( fp, "%g %g\n", cp->u.graf.points[0][0],
					cp->u.graf.points[1][0] );
		} else {
			fprintf( fp, "%d\n", cp->u.graf.np );
			for ( i=0 ; i < cp->u.graf.np ; i++ ) {
				fprintf( fp, "%g %g\n", cp->u.graf.points[i][0],
					cp->u.graf.points[i][1] );
			}
		}
	}
}


static void
cut_write( int cid, char * const filename )
{
	Cut		*p;
	FILE		*fp;
	int		i;
	extern int	errno;

	fp = fopen( filename, "w" );
	if ( fp == 0 ) {
		sf_report( "Cannot open %s for writing (%s)\n", filename,
			strerror( errno ) );
		return;
	}

	fprintf( fp, "# Paw Cut File V1.0\n" );

	if ( cid == 0 ) {
		for( i=1 ; i <= MAX_CUT_ID ; i++ ) {
			p = cut_list[ i ];
			if ( p != 0 ) {
				cut_write_one( fp, p );
			}
		}
	} else {
		p = cut_list[ cid ];
		if ( p != 0 ) {
			cut_write_one( fp, p );
		} else {
			sf_report( "$%d does not exist\n", cid );
		}
	}

	fclose( fp );
}


static void
cut_add( int cid, char * const expr )
{
	int	err = R_NOERR;
	Cut	*p;

	cut_new( cid, &err );
	if ( err != R_NOERR ) {
		return;
	}

	p = cut_list[ cid ];

	p->ctyp = C_EXPR;
	p->u.expr.expr = expr;
}


void
gcut_add_1d( int cid, const char * const expr, float xlow, float xhigh )
{
	int	err = R_NOERR;
	Cut	*p;

	cut_new( cid, &err );
	if ( err != R_NOERR ) {
		return;
	}

	p = cut_list[ cid ];

	p->ctyp = C_GRAF;
	p->u.graf.dim = 1;
	p->u.graf.np = 2;

	p->u.graf.points = (CutData) calloc( sizeof(float), 4);
	qp_assert( p->u.graf.points != 0 );

	p->u.graf.exprx = str_new( expr );

	p->u.graf.points[0][0] = xlow;
	p->u.graf.points[1][0] = xhigh;
}


void
gcut_add_2d( int cid, const char * const expr_x, const char * const expr_y,
	int n, float xv[], float yv[] )
{
	int	err = R_NOERR, i;
	Cut	*p;

	cut_new( cid, &err );
	if ( err != R_NOERR ) {
		return;
	}

	p = cut_list[ cid ];

	p->ctyp = C_GRAF;
	p->u.graf.dim = 2;
	p->u.graf.np = n;

	p->u.graf.points = (CutData) calloc( sizeof(float), 2 * n );
	qp_assert( p->u.graf.points != 0 );

	p->u.graf.exprx = str_new( expr_x );
	p->u.graf.expry = str_new( expr_y );

	for( i=0 ; i < n ; i++ ) {
		p->u.graf.points[i][0] = xv[i];
		p->u.graf.points[i][1] = yv[i];
	}
}


static void
gcut_draw( int cid )
{
	Cut	*p;

	p = cut_list[ cid ];

	if ( p == 0 ) {
		sf_report( "$%d does not exist\n", cid );
		return;
	}

	if ( p->ctyp != C_GRAF ) {
		sf_report( "$%d is not a graphical cut\n", cid );
		return;
	}

	if ( p->u.graf.dim == 1 ) {
		sf_report( "Cannot draw 1d cut\n" );
	} else {	/* 2 dimensional */
		char	opt[5];
		int	x=0, y=0, z=0, n, i;
		float	*xv, *yv;

		PAHLOG(x,y,z);

		if ( x || y ) {
			strcpy( opt, "LG" );
			if ( x ) strcat( opt, "X" );
			if ( y ) strcat( opt, "Y" );
		} else {
			strcpy( opt, "L" );
		}

		n = p->u.graf.np;
		xv = (float *) calloc( sizeof(float), n+1 ); qp_assert( xv );
		yv = (float *) calloc( sizeof(float), n+1 ); qp_assert( yv );

		for ( i=0 ; i < n ; i++ ) {
			xv[i] = p->u.graf.points[i][0];
			yv[i] = p->u.graf.points[i][1];
		}
		xv[i] = p->u.graf.points[0][0];
		yv[i] = p->u.graf.points[0][1];


		IGRAPH( n+1, xv, yv, opt );

		free( (void *) xv );
		free( (void *) yv );
	}
}


int
cut_get_cid( char * const cid_str )
{
	char	*num_str;	/* number of the cut */
	char	*cid_end;	/* end of the cut id */
	long	cid;	/* 0 (all cuts) or the requested cut */

	num_str = *cid_str == '$' ? cid_str+1 : cid_str;
	cid = strtol( num_str, &cid_end, 10 );
	if ( (num_str == cid_end) || (*cid_end != 0) ||
		(cid < 0) || (cid > MAX_CUT_ID) ){
		sf_report( "Illegal Cut ID '%s', use an integer in the range"
			" [1..%d].\n", cid_str, MAX_CUT_ID );
		cid = -1;
	}

	return cid;
}


void
pancuts_C( void )
{
	int	npar;		/* the number of parameters */
	int	cid;		/* the cut number */
	char	*cid_str;	/* the cut id argument */
	char	*opt_str;	/* the option/expression argument */
	char	*filename = 0;	/* the optional filename argument */
	int	wkid = 1;	/* the optional workstation identifier */

	if ( setjmp( qp_abort_env ) != 0 ) {
		return;	/* we had a serious problem */
	} else {
		qp_abort_env_valid = 1;
	}

	npar = ku_npar();

	if ( npar == 0 ) {
		cut_print( 0 );
		qp_abort_env_valid = 0;
		return;
	}

	cid_str = ku_gets();
	if ( (cid = cut_get_cid( cid_str )) == -1 ) {
		qp_abort_env_valid = 0;
		return;
	}

	if ( npar == 1 ) {
		cut_print( cid );
		qp_abort_env_valid = 0;
		return;
	}

	opt_str = str_new( ku_gets() );
	if ( npar > 2 ) {
		filename = ku_gets();
		if ( *filename ) {
			filename = str_new( filename );
		} else {
			filename = 0;
		}
	}

	if ( npar > 3 ) {
		wkid = ku_geti();
	}

	if ( npar > 4 ) {
		sf_report( "Unexpected argument(s).\n" );
		if ( filename ) str_del( filename );
		qp_abort_env_valid = 0;
		return;
	}

	if ( strcmp( opt_str, "P" )==0 || strcmp( opt_str, "p" )==0 ) {
		cut_print( cid );
	} else if ( strcmp( opt_str, "-" )==0 ) {
			cut_del( cid );
	} else if ( strcmp( opt_str, "R" )==0 || strcmp( opt_str, "r" )==0 ) {
		if ( filename == 0 ) {
			printf( "Filename missing\n" );
		} else {
			cut_read( cid, filename );
		}
	} else if ( strcmp( opt_str, "W" )==0 || strcmp( opt_str, "w" )==0 ) {
		if ( filename == 0 ) {
			printf( "Filename missing\n" );
		} else {
			cut_write( cid, filename );
		}
	} else if ( strcmp( opt_str, "D" )==0 || strcmp( opt_str, "d" )==0 ) {
		gcut_draw( cid );
	} else if ( strcmp( opt_str, "G" )==0 || strcmp( opt_str, "g" )==0 ) {
			printf( "Use command GCUT to create a graphical cut\n" );
	} else {
		cut_add( cid, opt_str );
	}

	/* just to be sure ... we do not want to come back here */
	qp_abort_env_valid = 0;
}


FCALLSCSUB0(pancuts_C,PANCUT,pancut)


/* a simple set of integers */

static UInt32	iset_mask[MAX_CUT_ID/32 + 1];

static void
iset_new()
{
	memset( iset_mask, 0, sizeof(iset_mask) );
}


static void
iset_add( int cid )
{
	int	index = cid / 32;
	UInt32	bit = 1 << ( cid % 32 );

	iset_mask[index] = iset_mask[index] | bit;
}


static bool
iset_contains( int cid )
{
	int	index = cid / 32;
	UInt32	bit = 1 << ( cid % 32 );

	return (iset_mask[index] & bit) != 0;
}


/* a simple queue of integers */


static int	iqueue_head;
static int	iqueue_tail;
static short	iqueue_list[MAX_CUT_ID];

static void
iqueue_new()
{
	iqueue_head = 0;
	iqueue_tail = 0;
}


static void
iqueue_add( int cid )
{
	iqueue_list[iqueue_head] = cid;
	iqueue_head = (iqueue_head + 1) % MAX_CUT_ID;
	qp_assert( iqueue_head != iqueue_tail );
}


static int
iqueue_del()
{
	int	cid;

	qp_assert( iqueue_head != iqueue_tail );
	cid = iqueue_list[iqueue_tail];
	iqueue_tail = (iqueue_tail + 1) % MAX_CUT_ID;

	return cid;
}


static bool
iqueue_empty()
{
	return iqueue_head == iqueue_tail ;
}



static void
check_recursion( int start, int * errp )
{
	bool	done;
	Cut	*p;
	int	cid, i;

	iqueue_new();
	iset_new();

	iset_add( start );
	iqueue_add( start );
	done = FALSE;

	while ( !done && !iqueue_empty() ) {
		cid = iqueue_del();
		p = cut_list[ cid ];	qp_assert( p != 0 );

		for ( i=0 ; i < p->num ; i++ ) {

			cid = p->dep[i];

			if ( cid == start ) {
				*errp = R_RECURSIVECUT;
				done = TRUE;
				break;
			}

			if ( ! iset_contains( cid ) ) {
				iset_add( cid );
				iqueue_add( cid );
			}
		}
	}
}


void
qp_cut_add_dep( int cid, int used_cid, int * errp )
{
	int	i;
	bool	found = FALSE;
	Cut	*p;

	p = cut_list[cid];

	if ( p->dep != 0 ) {
		for( i=0 ; i < p->num ; i++ ) {
			if ( p->dep[i] == used_cid ) {
				found = TRUE;
				break;
			}
		}
	}

	if ( ! found ) {
		if ( (p->dep == 0) || (p->size == p->num) ) {
			p->size += 5;
			if ( p->dep == 0 ) {
				p->dep = (short *) malloc(
					p->size * sizeof(short) );
			} else {
				p->dep = (short *) realloc( p->dep,
					p->size * sizeof(short) );
			}
			if ( p->dep == 0 ) {
				p->size = 0;
				*errp = R_ALLOC_ERROR;
				return;
			}
		}
		p->dep[p->num++] = used_cid;

	}

	check_recursion( cid, errp );

	if ( *errp == R_RECURSIVECUT ) {
		sf_report( "Cut $%d is recursively defined\n", cid );
	}
}
