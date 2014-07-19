/*
 *  qp_name.c  --
 *	Implement name lookup and resolving.
 *
 *  Original: 10-Oct-1994 13:59
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_name.c,v 1.33 1999/07/05 15:43:35 couet Exp $
 *
 *  $Log: qp_name.c,v $
 *  Revision 1.33  1999/07/05 15:43:35  couet
 *  - hbook_interface.h in now replaced by hbook.h in CVSCOSRC
 *
 *  Revision 1.32  1999/06/30 15:38:24  couet
 *  - comis_interface.h is now replaced by comis.h in CVSCOSRC
 *
 *  Revision 1.31  1999/06/28 15:09:04  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.30  1996/10/15 16:11:54  couet
 *  - Access to the first variable of a RWN in a UWFUNC generated function
 *    was wrong (VMEM 0 allows to bypass the problem).
 *
 *  Revision 1.29  1996/07/11 09:04:11  couet
 *  - The offset used to retrive COMIS variable was wrong (13 instead of 14)
 *  This causes the "know bug" number one in http://wwwcn.cern.ch/pl/paw/bugs.html
 *
 *  Revision 1.28  1996/06/10 14:56:06  dinofm
 *  The handling of COMIS compiled functions has been modified so that, if
 *  the process is a PIAF master, the 'master' shared library is loaded
 *  instead than recompiling the source (this would not affect the PAW client)
 *
 *  Revision 1.27  1996/05/24 09:16:07  dinofm
 *  Bug fixed when a FORTRAN selection function operates on a PIAF residing
 *  ntuple. The source file is sent to PIAF and compiled if '.f77' extension
 *  has been used.
 *
 *  Revision 1.26  1996/05/10 09:07:47  maartenb
 *  - Fixed resolving equivalences in comis functions.
 *
 *  Revision 1.25  1996/04/23 18:38:46  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>

#include	"str.h"	/* placed because of IRIX _INT in stdarg.h */
#include	<cfortran/cfortran.h>

#define CF_TRUE         C2FLOGICAL(1)
#define CF_FALSE        C2FLOGICAL(0)

#include	"c_hcflag.h"
#include	"c_quest.h"

#include	"c_hcpiaf.h"
#include	<cfortran/comis.h>
#include	"hbook_defs.h"
#include	<cfortran/hbook.h>
#include	"kuip_interface.h"
#include	"errors.h"
#include	"paw_interface.h"
#include	"qp_compile.h"
#include	"qp_mask.h"
#include	"qp_name.h"
#include	"qp_signature.h"
#include	"qp_symtab.h"
#include	"qp_report.h"
#include	"qpflags.h"
#include	"svec.h"


bool		db_name = FALSE;		/* control debug printing */


static char	chtitl[128];
static char	tags[MAX_RWN_COLS][9];
static float	rlow[MAX_RWN_COLS];
static float	rhigh[MAX_RWN_COLS];


static Signature *
name_rwntup( int curr_ntup, char * const name )
{
	Signature	*s;
	int		nvar, i;
	char		*p;

	s = 0;

	nvar = MAX_RWN_COLS;
	HGIVEN(curr_ntup,chtitl,nvar,tags,rlow,rhigh);

	if ( nvar == 0 ) {
		return s;
	}

	for( i=0 ; i < nvar ; i++ ) {

		for ( p = tags[i]; *p == ' ' ; p++ ) { }

		if ( strcasecmp( name, p ) == 0 ) {

			if ( db_name ) {
				printf( "RWN %d: %-8s [%g,%g] from '%s'\n",
					i+1, tags[i], rlow[i], rhigh[i],
					chtitl );
			}

			s = sig_new_rvar( i );
			break;
		}
	}

	return s;
}


/*
 *  Check if the name is part of the current ntuple
 *  and return the signature if it is
 */
static Signature *
name_ntup( int curr_ntup, char * const name )
{
	Signature *s;
	Dimension *dim;
	char	block[MAX_BLOCK_LEN+1], civar[MAX_NAME_LEN+1];
	int	itype, isize, ielem;
	int	nvdim, ivdim[MAX_DIMENSION], ilow, iup, ierr;
	int	i, r;
	STIndex	idx;

	s = 0;

	if ( HNTNEW( curr_ntup ) == 0 ) {
		return name_rwntup( curr_ntup, name );
	}

	block[0] = '\0'; civar[0] = '\0';
	HVXIST( name, block, civar, itype, isize, ielem);

	if ( ielem != 0 ) {
		HNTDIM( name, nvdim, ivdim, MAX_DIMENSION );

		ierr = -1;
		if ( itype == 2 || itype == 3 ) {
			HNTRNG( name, ilow, iup, ierr );
		}

		if ( db_name ) {
			printf( "<%s> in <%s> index <%s> type %d size %d "
				"elem %d\n", name, block, civar, itype, isize,
				ielem );
			if ( ierr == 0 ) printf( "[%d,%d]\n", ilow, iup );
			printf( "(" );
			for(i=0; i<nvdim ; i++ ) {
				if(i!=0)(void)putchar(','); printf( "%d",
				ivdim[i] );
			}
			printf( ")\n" );
		}
		switch ( itype ) {	/* switch int */
		case 1:
			if ( isize == 4 )
				dim = dim_new( D_FLOAT );
			else
				dim = dim_new( D_DOUBLE );
			break;
		case 2:
			if ( isize == 4 )
				dim = dim_new( D_INT );
			else
				dim = dim_new( D_LONG );

			if ( ierr == 0 ) {
				dim->isRange = TRUE;
				dim->ilow = ilow;
				dim->iup = iup;
			}
			break;
		case 3:
			if ( isize == 4 )
				dim = dim_new( D_UINT );
			else
				dim = dim_new( D_ULONG );

			if ( ierr == 0 ) {
				dim->isRange = TRUE;
				dim->ilow = ilow;
				dim->iup = iup;
			}
			break;
		case 4:
			dim = dim_new( D_BOOL ); break;
		case 5:
			dim = dim_new( D_STR );
			dim->strSize = isize;
			break;
		default:
			qp_abort( "name_ntup: HVXIST return unknown itype %d\n", itype );
			break;
		}

		dim->ndim = nvdim;
		for( i=0 ; i < nvdim ; i++ ) {
			dim->range[i] = ivdim[i];
			dim->max_range[i] = ivdim[i];
		}
		
		if ( civar[0] != '\0' ) {	/* XXXXX should link to */
						/* civar via symbol table */
			idx = st_index( sf_symtab, civar );
			if ( idx == -1 ) {
				idx = name_resolve( civar, &r );
				if ( idx == -1 ) {
					return 0;
				} 
			}
			dim->idx = idx;
		} else {
			dim->idx = STI_INVALID;
		}

		s = sig_new_cvar( dim, block, isize, ielem );
		dim_del( dim );
	}

	return s;
}


static Signature *
name_mask( char * const name )
{
	Mask		*mp;
	Signature	*s;

	s = 0;

	mp = mask_find( name );
	if ( mp != 0 ) {
		s = sig_new_mask( mp );
	}

	return s;
}


/*
 *  The following cases are supported by comis:
 *
 *  String in query		File(s)		Symbol
 *
 *  bla.f			bla.f		bla	interpreted
 *  bla.f77			bla.f		bla	interpreted and compiled/dynlinked
 *  bla.sl			bla.sl + bla.f	bla	interpreted and dynlinked
 *  bla.c			bla.c		bla	compiled/dynlinked
 *  bla.csl			bla.csl		bla	dynlinked
 */


typedef enum {
	LDT_Interpret = 1,
	LDT_Compile_F77,
	LDT_Compile_C,
	LDT_Load_F77,
	LDT_Load_sl
} LoadType;

typedef struct _load_type_map_ {
	LoadType	ltyp;
	char *		suffix;
} LoadTypeMap;


static LoadTypeMap	theLoadTypeMap[] = {
	{ LDT_Interpret,	".f" },
	{ LDT_Interpret,	".for" },
	{ LDT_Interpret,	".fortran" },
	{ LDT_Compile_F77,	".f77" },
	{ LDT_Compile_C,	".c" },
	{ LDT_Load_F77,		".sl" },
	{ LDT_Load_sl,		".csl" },
	{ 0,			0 }
};

static int
file_load(
	char *		fname
){
	FILE		*fp;
	int		iret, ildt;
	char		*suffix, *file, *command, *p;

	file = str_new( fname );
	suffix = strrchr( file, '.' );
	if ( suffix == 0 ) {
		sf_report( "Cannot determine suffix '%s'\n", fname );
		str_del( file );
		return R_LOAD_ERROR;
	}

	for ( ildt = 0 ; theLoadTypeMap[ildt].suffix != 0 ; ildt++ ) {
		if ( strcasecmp( suffix, theLoadTypeMap[ildt].suffix ) == 0 )
			break;
	}
	if ( theLoadTypeMap[ildt].suffix == 0 ) {
		str_del( file );
		sf_report( "Unknown suffix '%s'\n", suffix );
		return R_LOAD_ERROR;
	}

	*suffix = '\0';	/* remove suffix from file */

	switch ( theLoadTypeMap[ildt].ltyp ) {
	case LDT_Interpret:
	case LDT_Compile_C:
		str_del( file );
		file = str_new( fname );
		break;

	case LDT_Compile_F77:
	case LDT_Load_F77:
		file = str_merge( file, str_new( ".f" ), (char *)0 );
		break;

	case LDT_Load_sl:
		file = str_merge( file, str_new( ".sl" ), (char *)0 );
		break;

	default:
		qp_abort( "file_load: Internal error; Unknown LoadType %d\n",
			theLoadTypeMap[ildt].ltyp );
		break;
	}


	fp = fopen( file, "r" );
	if ( fp != 0 ) {
		fclose( fp );
	} else {
		sf_report( "Cannot open file '%s'\n", fname );
		return R_LOAD_ERROR;
	}

	if ( theLoadTypeMap[ildt].ltyp == LDT_Load_F77 ) {
		fp = fopen( fname, "r" );
		if ( fp != 0 ) {
			fclose( fp );
		} else {
			sf_report( "Cannot open file '%s'\n", fname );
			return R_LOAD_ERROR;
		}
	}

	if ( HCPIAF.ntpiaf == CF_TRUE ) {
	/* CSEXEC is first called locally when                        */
	/* the ntuple is on PIAF (this allows to 'COMIS compile' even */
	/* '.f77' functions without producing a local shared library) */
	/* The CSEXEC is then sent to the PIAF master who will execute*/
	/* it on the remote PIAF node                                 */
		command = str_merge( str_new("!file "), 
			str_new(file), (char *) 0 );
		CSEXEC( command, iret );

		if (theLoadTypeMap[ildt].ltyp == LDT_Compile_F77) {
			file = str_merge( file, str_new( "77" ), (char *)0 );
		}
		/* Send the file to Piaf and compile it on the remote node */
		PFCSEX( 0, file, iret);

	} else {
		if ( HCPIAF.servpf ) {	
		/* If the process is a Piaf master do not compile again */
		/* Since the library is already compiled just load it   */
		/* Tested for f77 functions but not for C functions !   */
			char buf[32];
			switch ( theLoadTypeMap[ildt].ltyp ) {
			case LDT_Compile_F77:
				suffix = strrchr( file, '.' );
				*suffix = '\0';
				sprintf( buf, "_%d.sl", HCPIAF.maspid );
				command = str_merge( str_new("!file "),
					str_new(file), str_new(buf), 
					(char *) 0 );
				break;
			case LDT_Compile_C:
				suffix = strrchr( file, '.' );
				*suffix = '\0';
				sprintf( buf, "_%d.csl", HCPIAF.maspid );
				command = str_merge( str_new("!file "),
					str_new(file), str_new(buf), 
					(char *) 0 );
				break;
			case LDT_Load_F77:
			case LDT_Load_sl:
			case LDT_Interpret:
				command = str_merge( str_new("!file "), 
					str_new(fname), (char *) 0 );
				break;
			}
		} else {
			command = str_merge( str_new("!file "), 
				str_new(fname), (char *) 0 );
		}
		CSEXEC( command, iret );
	}

	for ( p=fname ; *p != '\0' ; p++ )
		*p = toupper( *p );
	str_del( file );
	str_del( command );

	return iret == 0 ? R_NOERR : R_LOAD_ERROR;
}


#define	QUEUE_SIZE	32
static int *	queue_base;
static int	queue_size, queue_head, queue_tail;


static bool
queue_contains(
	int		cadr
){
	int		i;

	qp_assert( queue_base != 0 );

	for ( i=0 ; i < queue_head ; i++ ) {
		if ( queue_base[i] == cadr ) {
			return TRUE;
		}
	}

	return FALSE;
}


static void
queue_add(
	int		cadr
){
	qp_assert( queue_base != 0 );

	if ( queue_head == queue_size ) {
		queue_size += QUEUE_SIZE;
		queue_base = (int *) realloc( queue_base,
			queue_size * sizeof(int) );
		qp_assert( queue_base );
	}

	queue_base[queue_head++] = cadr;
}


static void
queue_start(
	int		cadr
){
	qp_assert( queue_base == 0 );
	queue_size = QUEUE_SIZE;
	queue_base = (int *) malloc( queue_size * sizeof(int) );
	qp_assert( queue_base );
	queue_head = 0;
	queue_tail = 0;

	queue_add( cadr );
}


static bool
queue_next(
	int *		cadrp
){
	qp_assert( queue_base != 0 );

	if ( queue_tail < queue_head ) {
		*cadrp = queue_base[queue_tail++];
		return TRUE;
	} else {
		return FALSE;
	}
}


static void
queue_free()
{
	qp_assert( queue_base != 0 );
	free( (void *) queue_base );
	queue_base = 0;
}


#define	LIST_SIZE	32
static char **	list_base;
static int	list_size, list_cur;

static void
list_reset()
{
	list_size = LIST_SIZE;
	list_base = (char **) malloc( list_size * sizeof(char *) );
	list_cur = 0;

	qp_assert( list_base );
}


static void
list_add(
	int		id,
	char *		name
){
	int		r = R_NOERR;
	Signature	*sig;

	if ( list_cur == list_size ) {
		list_size += LIST_SIZE;
		list_base = (char **) realloc( list_base,
			list_size * sizeof(char *) );
		qp_assert( list_base );
	}

	list_base[list_cur++] = str_new( name );

	/* add names to symbol table so qp_generate/... can find them */
	if ( st_index( sf_symtab, name ) == STI_INVALID ) {
		sig = name_ntup( id, name );
		r = st_insert( sf_symtab, name, sig );
		qp_assert( r == R_NOERR );
	}
}


static char **
list_val()
{
	char **		p;

	if ( list_cur == list_size ) {
		list_size += 1;
		list_base = (char **) realloc( list_base,
			list_size * sizeof(char *) );
		qp_assert( list_base );
	}

	if ( db_name ) {
		int	i;

		for ( i=0 ; i < list_cur ; i++ ) {
			sf_report( "COMIS Name: add %s because of comis usage\n",
				list_base[i] );
		}
	}

	list_base[list_cur++] = 0;

	p = list_base;
	list_base = 0;

	return p;
}


typedef struct _clist_ {
	int		item_size;
	char *		name;
} Clist;

Clist	clist_old[] = { {1, "PAWIDN"}, {0,0} };
Clist	clist_new[] = { {1, "PAWCR4"}, {2, "PAWCR8"}, {8, "PAWC32"}, {0,0} };


static void
comis_scan_subs(
	int		id,
	Signature *	sig,
	int *		r
){
	bool		new_ntuple;
	int		cadr, new_cadr;
	Clist		*cp_start, *cp;
	int		dummy1, dummy2, dummy3;
	int		ioffs, nel, idx, ierr, isize, next;
	char		var[MAX_NAME_LEN+1], *p;

	new_ntuple = HNTNEW( id );
	if ( new_ntuple ) {
		cp_start = clist_new;
	} else {
		cp_start = clist_old;
	}
	cadr = sig->u.comis.iaddr;

	queue_start( cadr );
	list_reset();

	while ( *r == R_NOERR && queue_next( &cadr ) ) {
		for ( cp = cp_start ; *r == R_NOERR && cp->name != 0 ; cp++ ) {
			isize = cp->item_size;
			ioffs = -1;
			CSLISTX( cp->name, cadr, ioffs, nel );
			/* ioffs in storage words and starting from 0 */
			while ( *r == R_NOERR && nel > 0 ) {
				while ( *r == R_NOERR && nel > 0 ) {
					if ( new_ntuple ) {
						idx = 1 + ioffs / isize;
						var[0] = '\0';
						HNTGETI(id,4*isize,idx,var,
							dummy1,dummy2,dummy3,
							ierr);
						if ( ierr == 0 ) {
							list_add( id, var );
							nel -= (dummy2/4) * dummy3;
							ioffs += isize * dummy3;
						} else { 
							sf_report( "Comis: Cannot find variable.\n\tIllegal equivalence ?? (Only first element of NTUPLE variable allowed)\n" );
							nel -= (dummy2/4);
							ioffs += isize;
							*r = R_NAME_UNDEF;
						}
					} else {
						if ( ioffs>13 ) {
							int nvar = MAX_RWN_COLS;
							HGIVEN(id,chtitl,nvar,
							tags,rlow,rhigh);
							if ( ioffs-14 < nvar ) {
							for ( p=tags[ioffs-14];
								*p==' '; p++ ){}
							list_add( id, p );
							} 
						}
						nel -= 1;
						ioffs += 1;
					}
				}

				CSLISTX( cp->name, cadr, ioffs, nel );
			}
		}

		if ( *r == R_NOERR ) {
			for( next = 0; (new_cadr = CSLISTC(cadr,next)) != 0 ;) {
				if ( ! queue_contains( new_cadr ) ) {
					queue_add( new_cadr );
				}
			}
		}
	}

	sig->u.comis.namelist = list_val();
	queue_free();

}


static Dimension *
comis_cstype( char * ident )
{
	int		cadr;
	bool		ok;
	Dimension	*d;
	char		chtype[2];


	cadr = CSADDR( ident );

	chtype[0] = '\0';
	CSTYPE( cadr, chtype );
	ok = TRUE;
	d = 0;
	switch ( chtype[0] ) {	/* switch char */
	case 'I': /* for integer function */
		d = dim_new( D_INT );
		break;
	case 'R': /* for real    function */
		d = dim_new( D_FLOAT );
		break;
	case 'C': /* for character function */
		sf_report( "Function returning string not supported"
				" ('%s')\n", ident );
		ok = FALSE;
		break;
	case 'L': /* for logical function */
		d = dim_new( D_BOOL );
		break;
	case 'D': /* for double pr. function */
		d = dim_new( D_DOUBLE );
		break;
	case 'S': /* for subroutine */
		sf_report( "Routine '%s' is a subroutine, should be a function\n",
				ident );
		ok = FALSE;
		break;
	case 'X': /* for complex function */
		sf_report( "Function returning complex not supported"
				" ('%s')\n", ident );
		ok = FALSE;
		break;
	case '?': /* for unknown or error */
	default:
		sf_report( "Internal error; CSTYPE returned '%c'\n", chtype[0]);
		ok = FALSE;
		break;
	}

	if ( ok && (d == 0) ) {
		sf_report( "comis_fun_type: Allocation error\n" );
		return 0;
	}

	return d;
}


static Dimension *
comis_cstypar( char *ident, int ipar, int * ierr )
{
	Dimension	*d = 0;
	int		cadr, isize, err, i;
	int		nvdim, dim[MAX_DIMENSION];
	char		chtype[2];
	bool		ok;

	cadr = CSADDR( ident );

	nvdim = MAX_DIMENSION;
	chtype[0] = '\0';
	CSTYPAR( cadr, ipar, chtype, isize, nvdim, dim, err );
	*ierr = err;
	if ( err == 0 ) {
		ok = TRUE;
		switch ( chtype[0] ) {	/* switch char */
		case 'I': /* for integer */
			d = dim_new( D_INT );
			break;
		case 'R': /* for real */
			d = dim_new( D_FLOAT );
			break;
		case 'C': /* for character */
			d = dim_new( D_STR );
			d->strSize = isize;
			break;
		case 'L': /* for logical */
			d = dim_new( D_BOOL );
			break;
		case 'D': /* for double pr. */
			d = dim_new( D_DOUBLE );
			break;
		case 'X': /* for complex */
			sf_report( "complex arguments not supported"
					" ('%s')\n", ident );
			ok = FALSE;
			break;
		case '?': /* for unknown or error */
		default:
			sf_report( "Internal error; CSTYPAR returned '%c'\n", chtype[0]);
			ok = FALSE;
			break;
		}

		if ( ok ) {
			if ( d == 0 ) {
				sf_report(
					"comis_fun_type: Allocation error\n" );
				*ierr = -1;
				return 0;
			}

			d->ndim = nvdim;
			for ( i=0 ; i < nvdim ; i++ ) {
				d->range[i] = dim[i];
				d->max_range[i] = dim[i];
			}
		}

	}

	return d;
}



/*
 *  Check if name is known by comis
 *  if name is an identifier just call comis,
 *  otherwise try to load as a file.
 */
static Signature *
name_comis( char * const name )
{
	Dimension	*d, *arg, *argv[MAX_DIMENSION];
	int		argc;
	Signature	*s;
	char		ident[MAX_NAME_LEN+1];
	char		*p, *fname;
	char		*fexpand(); /* from kuip */
	int		cadr, r, ierr, i;
	int		npar;
	bool		ok, loop;


	PAWCS;	/* initialize comis */

	s = 0;

	/* decide if name is an identifier or a filename */
	for( p=name ; *p != '\0' ; p++ ) {
		if ( !isalnum( *p ) && *p != '_' && *p != '$' )
			break;
	}

	if ( *p != 0 ) {	/* not an identifier, try to load the file */
				/* this can only be a file, so report probs */

		p = fexpand( name, (char *) 0 );	/* from kuip */
		fname = str_new( p ); free( p );
		if ( db_name ) {
			printf( "COMIS: file \"%s\"\n", fname );
		}

		/* load the file, based on the extension */
		r = file_load( fname );
		if ( r != R_NOERR ) {
			str_del( fname );
			return 0;
		}
		/* determine identifier */
#ifdef VMCMS
		/* take the first component */
		strncpy( ident, name, MAX_NAME_LEN ); ident[MAX_NAME_LEN] = '\0';
		p = strchr( ident, (int) '.' );
		if ( p != 0 )
			*p = '\0';
#else
		/* strip the suffix */
		p = strrchr( fname, '.' );
		if ( p == 0 )
			p = &fname[strlen(fname)];
		*p = '\0';

		/* scan backward */
		for( ; p > fname ; ) {
			char	*q = p - 1;
			if ( *q == '/' )
				break;	/* unix dir */
			if ( *q == ']' )
				break;	/* vms dir */
			if ( *q == ']' )
				break;	/* msdos dir */
			if ( *q == ':' )
				break;	/* vms/MacOS dir */
			p = q;
		}
		strncpy( ident, p, MAX_NAME_LEN ); ident[MAX_NAME_LEN] = '\0';
		str_del( fname );
#endif

		for( p=ident ; *p != '\0' ; p++ ) {
			if ( !isalnum( *p ) && *p != '_' && *p != '$' )
				break;
		}
		if ( *p != '\0' ) {	/* not a valid identifier */
			sf_report( "Cannot determine identifier from "
					"filename '%s'\n", fname );
			return 0;
		}

	} else {
		strncpy( ident, name, MAX_NAME_LEN); ident[MAX_NAME_LEN] = '\0';
	}

	for( p=ident ; *p != 0 ; p++ )
		*p = toupper( *p );

	cadr = CSADDR(ident);

	if ( cadr == 0 ) {
		return 0;
	}

	d = comis_cstype( ident );
	if ( d == 0 ) {
		return 0;
	}

	if ( db_name ) {
		dump_dimension( stdout, d );
		putchar('\n');
	}

	argc = 0;
	for ( npar=1, ok=TRUE, loop=TRUE; loop ; npar++ ) {

		arg = comis_cstypar( ident, npar, &ierr );
		if ( db_name ) {
			printf( "Arg %d (ierr=%d) : ", npar, ierr );
		}

		switch ( ierr ) {
		case 0:		/* OK */
			if ( npar > MAX_DIMENSION ) {
				sf_report( "Function %s has to many arguments"
					" (max %d)\n", ident, MAX_DIMENSION );
				ok = FALSE;
				loop = FALSE;
				dim_del( arg );
			} else if ( arg->dtyp == D_STR ) {
				sf_report( "Function %s, argument %d: "
					"Type character not supported\n",
					ident, npar );
				ok = FALSE;
				loop = FALSE;
				dim_del( arg );
			} else {
				if ( db_name ) {
					dump_dimension( stdout, arg );
					putchar('\n');
				}
				argv[npar-1] = arg;
				argc = npar;
			}
			break;

		case 2:		/* IPAR is wrong, no more args */
			loop = FALSE;
			break;
		
		case 3:		/* nvdim to small (reported later)*/
			sf_report( "Function %s, argument %d has to many"
				" dimensions (max %d)\n", ident, npar,
				MAX_DIMENSION );
			ok = FALSE;
			loop = FALSE;
			break;

		case 4:		/* No info on arguments */
			sf_report( "WARNING: %s has no type info\n", ident );
			argc = -1;
			loop = FALSE;
			break;

		case 1:		/* wrong cadr */
		default:
			sf_report( "name_comis: unexpected return code %d\n",
				ierr );
			ok = FALSE;
			loop = FALSE;
			break;
		}
	}
	if ( db_name ) {
		putchar('\n');
	}

	if ( ok ) {
		s = sig_new_comis( cadr, d, argc, argv );
	}

	dim_del( d );
	for ( i=0 ; i < argc ; i++ ) {
		dim_del( argv[i] );
	}

	return s;
}


static Signature *
name_vector( char * const name )
{
	Signature	*s;
	int		llow, lhigh, i;
	char		*t;
	Dimension	*d;
	Value		*v;

	s = 0;


	/* &PAWC.u.iq[llow-1] is the start of the vector */
	KUVECT(name,llow,lhigh);
	if ( llow == 0 ) {
		return 0;
	}

	if ( QUEST.iquest[13] == 1 ) {
		t = "real";
		d = dim_new( D_FLOAT );
	} else if ( QUEST.iquest[13] == 2 ) {
		t = "integer";
		d = dim_new( D_INT );
	} else {
		sf_report( "Unexpected type for kuip vector !\n" );
		return 0;
	}

	d->ndim = 1;
	d->range[0] = QUEST.iquest[30];
	d->range[1] = QUEST.iquest[31];
	if ( d->range[1] > 1 )
		d->ndim = 2;
	d->range[2] = QUEST.iquest[32];
	if ( d->range[2] > 1 )
		d->ndim = 3;
	d->isRange = FALSE;
	for ( i=0 ; i < d->ndim ; i++ ) {
		d->max_range[i] = d->range[i] ;
	}

	if ( db_name ) {
		printf( "vector:  %s type %s total len %d ", name, t,
			QUEST.iquest[10] );
		printf( "( %d:%d, %d:%d, %d:%d ) from %d to %d\n",
		QUEST.iquest[20], QUEST.iquest[30],
		QUEST.iquest[21], QUEST.iquest[31],
		QUEST.iquest[22], QUEST.iquest[32], llow, lhigh );
	}

	s = sig_new_vec( d );
	dim_del( d );

	return s;
}


STIndex
name_resolve( char * const name, int * r )
{
	Signature	*sig, *sig_nt, *sig_m, *sig_c, *sig_v;
	int		i, nsigs;
	STIndex		idx;
	SVec		found;

	db_name = qp_flags_get( "name" );

	qp_assert( st_index( sf_symtab, name ) == STI_INVALID );

	*r = R_NOERR;
	nsigs = 0;
	sig = 0; sig_nt = 0; sig_m = 0; sig_c = 0; sig_v = 0;

	found = svec_new(4);

	sig_nt = name_ntup( the_qs_record->id, name );
	if ( sig_nt != 0 ) {
		nsigs += 1;
		svec_add( found, str_new("- Ntuple variable") );
	}

	if ( sig == 0 ) {
		sig = sig_nt;
		sig_nt = 0;
	}

	sig_m = name_mask( name );
	if ( sig_m != 0 ) {
		nsigs += 1;
		svec_add( found, str_new("- Mask name") );
	}

	if ( sig == 0 ) {
		sig = sig_m;
		sig_m = 0;
	}

	sig_c = name_comis( name );
	if ( sig_c != 0 ) {
		nsigs += 1;
		svec_add( found, str_new("- Comis function") );
	}

	if ( sig == 0 ) {
		sig = sig_c;
		sig_c = 0;
	}

	sig_v = name_vector( name );
	if ( sig_v != 0 ) {
		nsigs += 1;
		svec_add( found, str_new("- Kuip vector") );
	}

	if ( sig == 0 ) {
		sig = sig_v;
		sig_v = 0;
	}

	switch( nsigs ) {	/* switch int */
	case 0:
		sf_report( "Name '%s' undefined\n", name );
		*r = R_TYPE_ERROR;
		idx = -1;
		break;
	case 1:
		/* proces signature */
		*r = st_insert( sf_symtab, name, sig );
		idx = st_index( sf_symtab, name );

		if ( sig->typ == S_COMIS ) {
			comis_scan_subs( the_qs_record->id, sig, r );
		}

		if ( *r != R_NOERR ) {
			idx = -1;
		}

		break;
	default:
		sf_report( "Name '%s' is defined more than once, it is a\n",
			name );
		for ( i=0 ; i < nsigs ; i++ ) {
			sf_report( "%s\n", svec_get( found, i ) );
		}
		*r = R_TYPE_ERROR;
		idx = -1;
		break;
	}

	if ( sig_nt != 0 ) sig_del( sig_nt );
	if ( sig_m != 0 ) sig_del( sig_m );
	if ( sig_c != 0 ) sig_del( sig_c );
	if ( sig_v != 0 ) sig_del( sig_v );
	svec_del( found );

	return idx;
}
