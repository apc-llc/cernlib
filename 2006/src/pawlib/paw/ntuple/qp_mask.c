/*
 *  qp_mask.c  --
 *	Implement the mask mechanism.
 *
 *  Original:  5-Oct-1995 16:28
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_mask.c,v 1.14 2000/02/10 11:29:33 couet Exp $
 *
 *  $Log: qp_mask.c,v $
 *  Revision 1.14  2000/02/10 11:29:33  couet
 *  - error message in case of old MASK format was wrong
 *
 *  Revision 1.13  1999/06/28 15:09:02  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.12  1996/10/17 15:45:08  couet
 *  - missing prototype for str_mask_name
 *
 *  Revision 1.11  1996/06/17 09:18:16  couet
 *  - The extraction of the MASK name from the file name was incorrect.
 *    This made impossible to use masks outside the current directory.
 *
 *  Revision 1.10  1996/05/15 13:20:10  maartenb
 *  - Modification to work around SunOS stdio bug.
 *
 *  Revision 1.9  1996/04/29 11:44:38  maartenb
 *  - Fix warnings about qsort() prototype.
 *
 *  Revision 1.8  1996/04/23 18:38:45  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<ctype.h>
#include	<errno.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"str.h"
#include	<cfortran/cfortran.h>

#include	"kuip_interface.h"
#include	"qp_report.h"
#include	"errors.h"
#include	"cern_types.h"
#include	"qp_mask.h"

/* extern functions */
extern char * str_mask_name(char *, int, int);

char *                ku_getf( void );
char *                ku_getc( void );
char *                ku_path( void );

/*
 *  The mask data structure ( intern to this file )
 */

typedef struct _ext_bits_ {
	int		selected;
	char		desc[80];
} ExtBits;


typedef struct _ext_header_ {		/* V1.0 */
	char		magic[16];
	char		mname[32];
	int		nevt;
	ExtBits		bits[32];
} ExtHeader;


#define	MASK_HASH_FREE		((Mask *) 0)
#define	MASK_HASH_DELETED	((Mask *) 1)

static Mask **	MaskTable;
static int	MaskTableBits = -1; /* MaskTable has 2**MaskTableBits slots */
static int	MaskTableCount;

static void mask_insert( Mask * mp );


static int
mask_read_header( Mask * mp )
{
	ExtHeader	*eh;
	int		i, n;

	qp_assert( sizeof(ExtHeader) <= 1024 * sizeof(Int32) );

	eh = (ExtHeader *) calloc( 1, sizeof(ExtHeader) );
	qp_assert( eh != 0 );

	fseek( mp->mfp, 0, SEEK_SET );

	n = fread( (void *) eh, sizeof(ExtHeader), 1, mp->mfp );

	if ( n == 1 ) {
		if (strncmp(eh->magic,"MASK FILE V 1.0",sizeof(eh->magic))!=0) {
			sf_report( "%s is not a mask file\n", mp->fname );
			sf_report( "Old style MASK, regenate the MASK\n");
			free( (void *) eh );
			return R_IOERROR;
		}

		if ( strncmp(mp->mname, eh->mname, sizeof(eh->mname)) != 0 ) {
			sf_report( "Warning %s contains a different mask name:"
				"'%s'\n", mp->fname, eh->mname );
		}

		mp->nevt = eh->nevt;
		for ( i=0 ; i < 32 ; i++ ) {
			if ( eh->bits[i].selected == -1 ) {
				mp->bits[i].valid = FALSE;
			} else {
				mp->bits[i].valid = TRUE;
				mp->bits[i].selected = eh->bits[i].selected;
				mp->bits[i].desc = str_subrange(
						eh->bits[i].desc, 0, 80 );
			}
		}
	} else {
		sf_report( "mask_read_header: Cannot read header for %s (%s)\n",
			mp->mname, strerror( errno ) );
	}

	free( (void *) eh );
	return n == 1 ? R_NOERR : R_IOERROR;
}


int
mask_flush_header( Mask * mp )
{
	ExtHeader	*eh;
	int		n, i;

	qp_assert( sizeof(ExtHeader) <= 1024 * sizeof(Int32) );
	qp_assert( mp->update );

	eh = (ExtHeader *) calloc( 1, sizeof(ExtHeader) );
	qp_assert( eh != 0 );

	fseek( mp->mfp, 0, SEEK_SET );

	/* fill the external header */

	strncpy( eh->magic, "MASK FILE V 1.0", sizeof(eh->magic) );
	strncpy( eh->mname, mp->mname, sizeof(eh->mname) );
	eh->nevt = mp->nevt;

	for ( i=0 ; i < 32 ; i++ ) {
		if ( mp->bits[i].valid ) {
			eh->bits[i].selected = mp->bits[i].selected;
			strncpy( eh->bits[i].desc,
				mp->bits[i].desc, sizeof(eh->bits[i].desc));
		} else {
			eh->bits[i].selected = -1;
			eh->bits[i].desc[0] = '\0';
		}
	}

	n = fwrite( (void *) eh, sizeof(ExtHeader), 1, mp->mfp );
	if ( n != 1 ) {
		sf_report( "mask_flush_header: Cannot write header for %s (%s)\n",
			mp->mname, strerror( errno ) );
	}

	free( (void *) eh );

	return n == 1 ? R_NOERR : R_IOERROR;
}


int
mask_flush_buf( Mask * mp )
{
	extern int	errno;
	int	n = 1;

	qp_assert( mp->update );

	if ( (mp->buf_index != -1) && mp->dirty ) {
		fseek( mp->mfp, (mp->buf_index + 1)*sizeof(mp->buf), SEEK_SET );
		n = fwrite( mp->buf, sizeof(mp->buf), 1, mp->mfp );
		mp->dirty = FALSE;
	}

	if ( n != 1 ) {
		sf_report( "mask_flush_buf: Cannot write buf %d for %s (%s)\n",
			mp->buf_index, mp->mname, strerror( errno ) );
	}
	return n == 1 ? R_NOERR : R_IOERROR;
}


static int
mask_read_buf( Mask * mp, int buf_index )
{
	extern int	errno;
	int		ierr, n;

	if ( mp->dirty ) {
		ierr = mask_flush_buf( mp );
		if ( ierr != R_NOERR ) {
			return ierr;
		}
	}

	mp->buf_index = buf_index;
	fseek( mp->mfp, (mp->buf_index + 1)*sizeof(mp->buf), SEEK_SET );
	n = fread( mp->buf, sizeof(mp->buf), 1, mp->mfp );

	if ( n != 1 ) {
		sf_report( "mask_read_buf: Cannot read buf %d for %s (%s)\n",
			buf_index, mp->mname, strerror( errno ) );
	}

	return n == 1 ? R_NOERR : R_IOERROR;
}


static void
mask_grow()
{
	int	i, n;
	Mask	**old, *mp;

	n = 1 << MaskTableBits;
	old = MaskTable;

	if ( old == 0 ) {
		MaskTableBits = 4;	/* first call */
	} else {
		MaskTableBits += 1;
	}

	MaskTable = (Mask **) calloc( 1 << MaskTableBits, sizeof( Mask *) );
	qp_assert( MaskTable != 0 );
	
	for( i=0; i < n ; i++ ) {
		mp = old[i];
		if ( (mp == MASK_HASH_FREE) || (mp == MASK_HASH_DELETED) )
			continue;
		mask_insert( mp );
	}

	if ( old != 0 ) {
		free( (void *) old );
	}
}


static void
mask_hash( const char * const mname, int * h1, int * h2 )
{
	register const char     *p;
	register unsigned       h = 0, g, c;
	int			u, m;

	for( p = mname; *p != 0 ; p++ ) {
		if ( isupper(*p) )
			c = tolower( *p );
		else
			c = *p;
		h = (h << 4) + c;
		if ( g = (h & 0xf0000000) ) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	m =  ((1 << MaskTableBits ) - 1);

	u = ( m - 2 - h ) % ( m - 2 );
	if ( (u & 1) == 0 ) u |= 1; 

	*h1 =  h & m;
	*h2 =  u;

}


Mask *
mask_find( const char * const mname )
{
	int	i, j, n, m;

	if ( MaskTable == 0 ) {
		mask_grow();
	}

	m =  1 << MaskTableBits;
	mask_hash( mname, &i, &j );

	for( n=0; n < m ; n++ ) {
		if ( MaskTable[i] == MASK_HASH_FREE ) {
			return 0;
		}

		if ( MaskTable[i] != MASK_HASH_DELETED ) {
			if ( strcasecmp( MaskTable[i]->mname, mname ) == 0 ) {
				return MaskTable[i];
			}
		}
		i = ( i + j ) % m;
	}

	qp_abort( "mask_find: Table Full ??\n" );
}


static void
mask_insert( Mask * mp )
{
	int	i, j, n, m;

	m =  1 << MaskTableBits;
	if ( (MaskTable == 0) || (MaskTableCount > ( 0.75 * m )) ) {
		mask_grow();
	}

	mask_hash( mp->mname, &i, &j );
	for( n=0; n < m ; n++ ) {
		if ( (MaskTable[i] == MASK_HASH_FREE) || 
			(MaskTable[i] == MASK_HASH_DELETED) ) {
			MaskTable[i] = mp;
			MaskTableCount += 1;
			return;
		}
		i = ( i + j ) % m;
	}

	qp_abort( "mask_insert: Table Full ??\n" );
}


static void
mask_delete( Mask * mp )
{
	int	i, j, n, m;

	if ( MaskTable == 0 ) {
		qp_abort( "mask_delete: No MaskTable ??\n" );
	}

	m =  1 << MaskTableBits;
	mask_hash( mp->mname, &i, &j );

	for( n=0; n < m ; n++ ) {
		if ( MaskTable[i] == MASK_HASH_FREE ) {
			qp_abort( "mask_delete: Name not found ??\n" );
		}

		if ( MaskTable[i] != MASK_HASH_DELETED ) {
			if ( strcmp( MaskTable[i]->mname, mp->mname ) == 0 ) {
				MaskTable[i] = MASK_HASH_DELETED;
				MaskTableCount -= 1;
				return;
			}
		}
		i = ( i + j ) % m;
	}

	qp_abort( "mask_delete: Table Full ??\n" );
}


static Mask *
mask_new( char * mname )
{
	Mask	*mp;

	mp = (Mask *) calloc( 1, sizeof(Mask) );
	qp_assert( mp != 0 );

	mp->mname = str_new( mname );

	mask_insert( mp );

	return mp;
}


static void
mask_free( Mask * mp )
{
	mask_delete( mp );
	str_del( mp->mname );
	free( (void *) mp );
}


static void
mask_close( Mask * mp )
{
	if ( mp->dirty ) {
		mask_flush_buf( mp );
	}

	if ( mp->update ) {
		mask_flush_header( mp );
	}

	fclose( mp->mfp );
	str_del( mp->fname );
	mask_free( mp );
}


static int
mask_read( FILE *mfp, char * fname, Mask *mp, char flag )
{
	mp->mfp = mfp;
	mp->fname = str_new( fname );
	mp->update = (flag == 'U') ? TRUE : FALSE;
	mp->dirty = FALSE;
	mp->buf_index = -1;

	return mask_read_header( mp );
}


static int
mask_init( FILE *mfp, char * fname, Mask *mp )
{
	int		i;

	mp->mfp = mfp;
	mp->fname = str_new( fname );
	mp->nevt = 0;
	mp->update = TRUE;
	mp->dirty = FALSE;
	mp->buf_index = -1;

	for ( i=0; i < 32 ; i++ ) {
		mp->bits[i].valid = FALSE;
	}

	return mask_flush_header( mp );
}


static void
mask_print( Mask * mp )
{
	int		i, n;
	char		*p;

	p = str_toupper( mp->mname );
	printf( "%-10s Events: %6d  (file %s, %s)\n", p, mp->nevt,
		mp->fname, mp->update ? "read/write" : "readonly" );
	str_del( p );

	for ( i=0, n=0 ; i < 32 ; i++ ) { if (  mp->bits[i].valid ) n += 1; }

	if ( n > 0 ) {
		printf( "            # select  Description\n" );
		for ( i=0 ; i < 32 ; i++ ) {
			if ( mp->bits[i].valid ) {
				printf( "    bit %2d: %8d  %s\n", i+1,
					mp->bits[i].selected, mp->bits[i].desc);
			}
		}
	} else {
		printf( "\tNo selections stored\n" );
	}
	putchar( '\n' );
}


typedef int ( *qsort_cmp_fun )( const void *, const void * );

static void
mask_print_all( void )
{
	char		**p;
	int		i, j, m;
	Mask		*mp;

	if ( (MaskTable == 0) || (MaskTableCount == 0) ) {
		printf( "No masks defined\n" );
		return;
	}

	p = (char **) calloc( MaskTableCount, sizeof(char *) );
	qp_assert( p != 0 );

	m =  1 << MaskTableBits;
	for( i=0, j=0 ; i < m ; i++ ) {
		mp = MaskTable[i];
		if ( (mp == MASK_HASH_FREE) || (mp == MASK_HASH_DELETED) )
			continue;
		p[j++] = mp->mname;
	}

	qp_assert( MaskTableCount == j );

	qsort( (void *) p, MaskTableCount, sizeof(char *),
		(qsort_cmp_fun) strcmp );

	for ( i=0 ; i < MaskTableCount ; i++ ) {
		mp = mask_find( p[i] );
		mask_print( mp );
	}

	free( p );
}


static void
mask_reset( Mask * mp, int bit )
{
	sf_report( "mask_reset: not implemented\n" );
}


void
mask_set_desc( Mask *mp, int ibit, char * desc )
{
	qp_assert( (1<=ibit) && (ibit<=32) );

	if ( mp->bits[ibit-1].valid ) {
		str_del( mp->bits[ibit-1].desc );
	}

	mp->bits[ibit-1].desc = str_new( desc );
	mp->bits[ibit-1].valid = TRUE;
	mp->bits[ibit-1].selected = 0;

	mask_flush_header( mp );
}


bool
mask_bit_defined( const Mask * const mp, const int ibit )
{
	qp_assert( (1<=ibit) && (ibit<=32) );

	return mp->bits[ibit-1].valid;
}


bool
mask_writable( const Mask * const mp )
{
	return mp->update;
}


UInt32
mask_get_word( Mask * mp, long ievt, int * errp )
{
	int	buf_index;

	if ( ievt > mp->nevt ) {
		sf_report( "Mask %s filled until event %d\n",
			mp->mname, mp->nevt );
		*errp = R_IOERROR;
		return 0;
	}

	buf_index = ievt / MASK_BLOCK_SIZE;

	if ( buf_index  != mp->buf_index ) {
		if ( mp->dirty ) {
			*errp = mask_flush_buf( mp );
		}
		if ( *errp == R_NOERR ) {
			*errp = mask_read_buf( mp, buf_index );
		}
		if ( *errp != R_NOERR ) {
			return 0;
		}
	}

	return mp->buf[ ievt % MASK_BLOCK_SIZE ];
}


void
mask_set_bit( Mask * mp, long ievt, int ibit, bool value, int * errp )
{
	int	buf_index;
	int	last_index;

	buf_index = ievt / MASK_BLOCK_SIZE;

	if ( buf_index  != mp->buf_index ) {
		if ( mp->dirty ) {
			*errp = mask_flush_buf( mp );
		}
		if ( *errp != R_NOERR ) {
			return;
		}

		last_index = mp->nevt / MASK_BLOCK_SIZE;
		if ( (mp->nevt > 0) && (buf_index <= last_index) ) {
			*errp = mask_read_buf( mp, buf_index );
		} else {
			memset( (void *) mp->buf, 0, sizeof(mp->buf) );
			mp->buf_index = buf_index;
		}
		if ( *errp != R_NOERR ) {
			return;
		}
	}

	if ( value ) {
		mp->buf[ ievt % MASK_BLOCK_SIZE ] |= 1 << (ibit-1);
		mp->bits[ibit-1].selected += 1;
	} else {
		mp->buf[ ievt % MASK_BLOCK_SIZE ] &= ~(1 << ibit-1);
	}

	mp->dirty = TRUE;

	if ( mp->nevt < ievt ) {
		mp->nevt = ievt;
	}
}


void
do_mask_open( void )
{
	Mask	*mp;
	FILE	*mfp;
	char	*fname, *mname, *option, *flag, *p;
	int	n, ierr;

	fname = str_new( ku_getf() );
	option = str_new( ku_getc() );

	if ( (strlen( option ) != 1) || (strchr("NRU", option[0]) == 0) ) {
		sf_report( "Option should be one of N,U or R (not '%s')\n",
		option );
		str_del( fname );
		str_del( option );
		return;
	}

	n = strlen( fname );
	if ( (n < 6) || (strcasecmp(&fname[n-5], ".mask") != 0)  ) {
		fname = str_merge( fname, str_new(".mask"), (char *) 0 );
		n += 5;
	}

	p = &fname[n-6];
	while ( (p > fname) && (isalnum(*p) || (*p == '_')) ) {
		p -= 1;
	}

	p = str_mask_name( fname, p - fname, n - 5 );
	mname = str_tolower( p ); str_del( p );

	mp = mask_find( mname );
	if ( mp != 0 ) {
		sf_report( "Closing mask %s (%s)\n", mname, mp->fname );
		mask_close( mp );
	}

	switch ( *option ) {
		case 'R': flag = "rb"; break;
		case 'N': flag = "w+b"; break;
		case 'U': flag = "r+b"; break;
		default: qp_abort("No Default\n");
	}

	/* work around: sunos where "wb+" is not opening the fil for reading */
/*
	if ( *option == 'N' ) {
		close( creat( fname, 0666 ) );
	}
*/

	mfp = fopen( fname, flag );
	if ( mfp == 0 ) {
		extern int	errno;

		sf_report( "Cannot open '%s', %s\n", fname, strerror(errno) );
		str_del( fname );
		str_del( mname );
		str_del( option );
		return;
	}

	mp = mask_new( mname );
	if ( *option == 'N' ) {
		ierr = mask_init( mfp, fname, mp );
	} else {
		ierr = mask_read( mfp, fname, mp, *option );
	}

	if ( ierr != R_NOERR ) {
		mask_free( mp );
		fclose( mfp );
	}

	str_del( fname );
	str_del( mname );
	str_del( option );
}


void
do_mask_close( void )
{
	Mask *	mp;
	char	*mname;

	mname = str_new( ku_getf() );

	mp = mask_find( mname );
	if ( mp != 0 ) {
		mask_close( mp );
	} else {
		sf_report( "Mask %s is not open\n", mname );
	}

	str_del( mname );
}


void
do_mask_list( void )
{
	Mask *	mp;
	int	npar;
	char	*mname;

	mname = str_tolower( ku_getf() );

	if ( strcmp( mname, "*" ) == 0 ) {
		mask_print_all();
	} else {
		mp = mask_find( mname );
		if ( mp != 0 ) {
			mask_print( mp );
		} else {
			sf_report( "Mask %s is not open\n", mname );
		}
	}
}


void
do_mask_reset( void )
{
	char	*mname;
	int	ibit;
	Mask	*mp;

	mname = str_tolower( ku_getf() );
	ibit = ku_geti();

	mp = mask_find( mname );
	if ( mp == 0 ) {
		sf_report( "Mask %s is not open\n", mname );
		str_del( mname );
		return;
	}

	if ( ! mp->update ) {
		sf_report( "Mask %s is not open for writing\n", mname );
		str_del( mname );
		return;
	}

	mask_reset( mp, ibit );
}


void
do_mask_convert( void )
{
	char	*mname, *oname;

	oname = str_tolower( ku_getf() );
	mname = str_tolower( ku_getf() );

	sf_report( "Convert of old masks not yet supported\n", mname );
}


void
npamask_C( void )
{
	char		*cmd_path;
	char		*cmd_string;

	if ( setjmp( qp_abort_env ) != 0 ) {
		return;	/* we had a serious problem */
	} else {
		qp_abort_env_valid = 1;
	}

	cmd_path = str_new( ku_path() );
	cmd_string = strrchr( cmd_path, '/' );
	if ( cmd_string != 0 ) {
		cmd_string += 1;
	} else {
		cmd_string = cmd_path;
	}

	if ( strcasecmp( "file", cmd_string ) == 0 ) {
		do_mask_open();
	} else if ( strcasecmp( "close", cmd_string ) == 0 ) {
		do_mask_close();
	} else if ( strcasecmp( "list", cmd_string ) == 0 ) {
		do_mask_list();
	} else if ( strcasecmp( "reset", cmd_string ) == 0 ) {
		do_mask_reset();
	} else if ( strcasecmp( "convert", cmd_string ) == 0 ) {
		do_mask_convert();
	} else {
		printf( "*** Internal error: %s unknown\n", cmd_string );
	}

	/* just to be sure ... we do not want to come back here */
	qp_abort_env_valid = 0;
}


FCALLSCSUB0(npamask_C,PANMAS,panmas)	/* create fortran entry point */
