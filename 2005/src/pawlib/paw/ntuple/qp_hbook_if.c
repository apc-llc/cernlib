/*
 *  qp_hbook_if.c  --
 *	Implementation of convenience routines for hbook.
 *
 *  Original:  8-May-1995 22:55
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_hbook_if.c,v 1.34 1999/07/05 15:43:35 couet Exp $
 *
 *  $Log: qp_hbook_if.c,v $
 *  Revision 1.34  1999/07/05 15:43:35  couet
 *  - hbook_interface.h in now replaced by hbook.h in CVSCOSRC
 *
 *  Revision 1.33  1999/07/01 14:47:01  couet
 *  - cspack_interface.h is replaced by cspack.h in CVSCOSRC
 *
 *  Revision 1.32  1999/06/28 15:09:02  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.31  1997/03/11 11:23:23  couet
 *  - In h_load_nt, the str_del(id_path) are removed because it is done in hntld_C
 *  - In hntld_C the pointer to the string (idpath) is intialized with 0 and the
 *    str_del is done only if the pointer is non zero.
 *
 *    This problems apeared on OSF when nt/print was done on a ntuple which doesn't
 *    exist.
 *
 *  Revision 1.30  1997/03/10 17:03:31  couet
 *  - in h_load_nt iquest(1) was not set at the right place when the ntuple
 *  doesn't exit
 *
 *  Revision 1.29  1996/09/12 09:28:56  couet
 *  - after NTUPLE commands using a full path name ie:
 *
 *    NT/PLOT //lun1/10.x
 *
 *    The current working directory was changed to //lun1
 *
 *  Revision 1.28  1996/09/11 15:01:09  couet
 *  - OLD_PATH is reset in h_load_nt when it exist in errors
 *  - New routine HRSTD to reset OLD_PATH from f77
 *
 *  Revision 1.27  1996/06/27 15:41:32  couet
 *  - Dummy command F/UNIT suppressed (ambiguous with K/UNITS)
 *
 *  Revision 1.26  1996/06/25 15:16:23  couet
 *  - C comment in CVS comments are not allowed !!!
 *
 *  Revision 1.25  1996/06/24 08:50:38  couet
 *  - Bug fix from Dong Zhao <dongzhao@d0sgi0.fnal.gov>:
 *
 *  I got two problems when I used the new Version of PAW/PIAF (96a) from CERN.
 *
 *  First, when I use a large Column Wise Ntuple, (400 variables, 8176 words of
 *  buffer space, 8191 words of record length), it showed:
 *
 *  PAW [1] hi/file 1 /proj5/qcdwz1/zee_1b/v9/data/zee_1b_1214_02.ntp_v9 0
 *  PAW [2] nt/pl 10.z_mass ! 1000
 *   ***** ERROR in HNBUFR : Not enough space in memory : ID=      10
 *
 *  The reason is that in this new version of PAW, HBBUFR reserves space for all
 *  the variables, in my case, 400X(8176+33) > 2000000 PAWC , while in the older
 *  versions, they call a routine called "hgetnt2" in which
 *  "hbname(idn,' ',0,'$CLEAR')" is called to reset the variable addresses.  In the
 *  new version, h_load_nt (corresponds to the old hgetnt) doesn't call hbname to
 *  reset the addresses of the variables.
 *
 *  The fix would be adding a line:
 *
 *          if ( ! PNTOLD.ntold )  HBNAME( idn, " ", 0, "$CLEAR" );
 *
 *  in the end of routine h_load_nt, right before line
 *
 *          * done *
 *
 *  Revision 1.24  1996/05/24 09:14:25  dinofm
 *  Memory leakage on id_path fixed
 *
 *  Revision 1.23  1996/05/17 15:52:12  dinofm
 *  Fixed a bug in h_load_nt. When the ntuple was on Piaf the HGETNT command
 *  was passed to CZPUTA through a common area mapped by cfortran.h.
 *  This caused CZPUTA to crash on AIX, OSF and possibly VMS. I removed any
 *  code related to the mapped common area and just passed the C buffer to
 *  the CZPUTA Fortran wrapper (tested on HP-UX & AIX).
 *
 *  Revision 1.22  1996/05/14 12:23:27  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.21  1996/05/10 12:23:27  dinofm
 *  PIAF debug messages (Ntuple on Piaf etc.) are issued only if the 'verbose'
 *  flag is not 0.
 *
 *  Revision 1.20  1996/05/02 08:06:24  dinofm
 *  Another fix about PIAF chains (ntuple path was used instead than ntuple id
 *  when composing the HGETNT command).
 *
 *  Revision 1.19  1996/04/26 13:21:47  maartenb
 *  - Fixed? the case where we get the number of entries from a chain on piaf
 *
 *  Revision 1.18  1996/04/23 18:38:43  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<string.h>

#include	"str.h"

#include	<cfortran/cfortran.h>

#include	"c_hcbits.h"
#include	"c_hcbook.h"
#include	"c_hcdire.h"
#include	"c_hcntpar.h"
#include	"c_hcpiaf.h"
#include	"c_pawchn.h"
#include	"c_pawcom.h"
#include	"c_pntold.h"
#include	"c_quest.h"
#include	<cfortran/cspack.h>
#include	"errors.h"
#include	<cfortran/hbook.h>
#include	"kuip_interface.h"
#include	"paw_interface.h"
#include	"qp_hbook_if.h"
#include	"qp_report.h"
#include	"qp_command.h"	/* for MAX_MAP_STRINGS */
#include	"qpflags.h"


#define	CF_TRUE		C2FLOGICAL(1)
#define	CF_FALSE	C2FLOGICAL(0)


char	old_path[MAX_RZ_PATH];

int
h_load_nt(
	char *		id_string,
	char **		id_path,
	int *		idp
) {
	char	idtmp_path[MAX_RZ_PATH];
	char	path[MAX_RZ_PATH], *p, *q;
	int	idn, idtmp;
	int	icycle;
	int	ierr;
	int     izero = 0;

	/* split string into  path, id and cycle */

	p = str_new( id_string );

	q = strrchr( p, ';' );
	if ( q != 0 ) {
		if ( str_atoi( q+1, 10, &icycle ) ) {
			sf_report( "Illegal cycle '%s'\n", q+1 );
			str_del( p );
			return R_SYNTAX_ERROR;
		}
		*q = '\0';
	} else {
		icycle = 9999;
	}
	q = strrchr( p, '/' );
	if ( q != 0 ) {
		if ( str_atoi( q+1, 10, &idn ) ) {
			sf_report( "Illegal IDN '%s'\n", q+1 );
			str_del( p );
			return R_SYNTAX_ERROR;
		}
		*q = '\0';
		strncpy( path, p, MAX_RZ_PATH-1 );
		path[MAX_RZ_PATH-1] = '\0';
		for( q=path ; *q != '\0' ; q++ ) {
			*q = toupper( *q );
		}
	} else {
		if ( str_atoi( p, 10, &idn ) ) {
			sf_report( "Illegal IDN '%s'\n", p );
			str_del( p );
			return R_SYNTAX_ERROR;
		}
		path[0] = '\0';
	}
	str_del( p );

	PAWCOM.id = idn; /* backward compatibility */
	if ( qp_flags_get( "verbose" ) != 0 ) {
		sf_report( "Loading Ntuple:  path='%s'  id=%d  icycle=%d\n", path,
			idn, icycle );
	}

	/* store the old path */

	HCDIR( old_path, "R" );

	/* cd to new dir / setup chain if necessary */
	PCHNCD( path, ierr );
	if ( ierr == 2 ) {
		QUEST.iquest[0] = -3;
		return R_SYNTAX_ERROR;
	}
	HCDIR( path, " " );
	if ( QUEST.iquest[0] != 0 ) {
		return R_SYNTAX_ERROR;
	}


	/* is the ntuple on piaf ? */
	if ( (HCDIRN.ichtop[HCDIRN.icdir-1] > 1000) &&
		(HCDIRN.ichlun[HCDIRN.icdir-1] != 0) ) {
		HCPIAF.ntpiaf = CF_TRUE;
		if ( qp_flags_get( "verbose" ) != 0 ) {
			sf_report( "Ntuple is on piaf\n" );
		}
	} else {
		HCPIAF.ntpiaf = CF_FALSE;
	}

	/* the id of the ntuple in memory will be : */
	idtmp = idn + PAWCOM.jofset;

	/* Check if the working directory is different from //pawc */
	if ( HCDIRN.ichtop[HCDIRN.icdir-1] != 0 ) {

		/* delete existing copy if necessary */
		if ( HEXIST(idtmp) ) {
			HFIND( idtmp, "h_load_nt" );
			HDCOFL();
			if ( HCBITS.i4 == 0 ) {
				/* idtmp is not an ntuple */
				strcpy( idtmp_path, "???" );
			} else {
				/* get path of idtmp */
				idtmp_path[0] = '\0';
				HGTDIR( idtmp_path );
			}
			if ( 
				( (path[0] != '\0') &&
				(strcmp(path,idtmp_path) != 0) )
			||
				( (path[0] == '\0') &&
				(strcmp(old_path,idtmp_path) != 0) )
			||
				PAWCHN.chain	) {
				HDELET( idtmp );
				HRIN( idn, icycle, PAWCOM.jofset );
			}
		} else {
			/* just load the ntuple into //pawc */
			HRIN( idn, icycle, PAWCOM.jofset );
		}
	}

	/* setup pointers and flags */
	HFIND( idtmp, "h_load_nt" );
	if ( HCBOOK.lcid == 0 ) {
		/* not found ... */
                HCDIR( old_path, " " );
		QUEST.iquest[0] = -1;
		return R_SYNTAX_ERROR;
	}
	HDCOFL();

	if ( HCBITS.i4 == 0 ) {
		ku_alfa();
		sf_report( "ID %d is not an ntuple.\n", idn );
                HCDIR( old_path, " " );
		QUEST.iquest[0] = -2;
		return R_SYNTAX_ERROR;
	}

	idtmp_path[0] = '\0';
	HGTDIR( idtmp_path );
	*id_path =str_new( idtmp_path );
	*idp = idn;

	HCDIR( path, "R" );	/* retrieve path, because of chain ?? */

	PNTOLD.ntold = ! HNTNEW( idtmp );

	/* check for needed recover */

	if ( PNTOLD.ntold ) {
		if ( PAWC.lq_eqv[HCBOOK.lcid-1-1] <= 0 ) {
			HDELET( idtmp );
			sf_report( "ID %d: Bad header. Try command RECOVER\n",
				idn );
                        HCDIR( old_path, " " );
			QUEST.iquest[0] = -4;
			return R_SYNTAX_ERROR;
		}
	} else {
		if ( PAWC.lq_eqv[HCBOOK.lcid+3-1] <= 0 ) {
			HDELET( idtmp );
			sf_report( "ID %d: Bad header. Try command RECOVER\n",
				idn );
                        HCDIR( old_path, " " );
			QUEST.iquest[0] = -4;
			return R_SYNTAX_ERROR;
		}
	}

	/* determine number of rows (chains/piaf !) */

	if ( PAWCHN.chain && ! HCPIAF.ntpiaf ) {
		PAWCHN.nchevt = PCHEVT( path, strlen(path), idn, 0, 2);

		if ( PAWCHN.nchevt == 0 ) {
			int	nchrow, nrow, iend;

			nchrow = 0;
			nrow = PAWC.iq_eqv[HCBOOK.lcid+3-1];
			for ( iend=0 ; iend == 0 ; ) {
				int	ncol;
				PCNEXT( idtmp, nchrow, ncol, nrow, iend );
			}
		}
	} 

	if ( PAWCHN.chain && HCPIAF.ntpiaf ) {

		PAWCHN.nchevt = PCHEVT( path, strlen(path), idn, 0, 2);

		if ( PAWCHN.nchevt == 0 ) {
			char	buf[81];
			PFSOCK( 0 );
			sprintf( buf, "HGETNT:%s", id_string );
			buf[80] = '\0';

			CZPUTA( buf, ierr );
			HLOGPF( " ", ierr );
			PAWCHN.nchevt = PCHEVT(
				path, strlen(path), idn, 
				QUEST.iquest[0], 1 );
		}
	} 

        if ( ! PNTOLD.ntold )  HBNAME( idn, " ", izero, "$CLEAR" );

	/* done */

	return R_NOERR;
}


static void
hntld_C( char * name )
{
	int	dummy_id;
	char	*dummy_path;

	dummy_path = 0;

	h_load_nt( name, &dummy_path, &dummy_id );

	if (dummy_path) str_del( dummy_path );

	return;
}

FCALLSCSUB1(hntld_C,HNTLD,hntld,STRING)


static void
hrstd_C( )
{
/* Restore the old path stored by h_load_nt from fortran */

	HCDIR( old_path, " " );

	return;
}

FCALLSCSUB0(hrstd_C,HRSTD,hrstd)

void
h_reset_dir( )
{
/* Restore the old path stored by h_load_nt from C */

	HCDIR( old_path, " " );

	return;
}


int
h_load_histo( char *id_string, int * idp, int *id_dim )
{
	char	*p;
	long	id;
	int	err = R_NOERR;

	id = strtol( id_string, &p, 10 );

	if ( (strlen(id_string) == 0) || (*p != 0) ) {
		return R_SYNTAX_ERROR;
	}

	HFIND( id, "h_load_histo" );
	if ( HCBOOK.lcid == 0 ) {
		return R_NAME_UNDEF;
	}

	HDCOFL();

	if ( (HCBITS.i1 != 1) && (HCBITS.i2 !=1) && (HCBITS.i8 != 1) ) {
		return R_NO_HISTOGRAM;
	}

	*idp = id;

	if ( (HCBITS.i1 == 1) || (HCBITS.i8 == 1) ) {
		*id_dim = 1;
	} else {
		*id_dim = 2;
	}

	return err;
}


/*
 *  Return the number of rows in a ntuple or chain
 */
void
h_hnoent( int idn, bool use_chain, int * ep )
{
	int	entries;

	if ( use_chain && PAWCHN.chain ) {
		*ep = PAWCHN.nchevt;
	} else {
		HNOENT( idn, entries );
		*ep = entries;
	}
}


/*
 *  Return the number of columns in a ntuple
 */
void
h_hnocol( int idn, int * colp )
{
	HFIND(idn,"HNOENT");

	qp_assert( HCBOOK.lcid != 0); /* found ntuple */
	qp_assert((PAWC.iq_eqv[HCBOOK.lcid+KBITS-1]&0x8)==0x8); /* is ntuple */

	*colp = PAWC.iq_eqv[HCBOOK.lcid+ZNDIM-1];
}


bool
h_flag_1d(
	int		id
){
	HFIND(id,"h_flag_1d");

	if ( HCBOOK.lcid == 0 ) {
		sf_report( "h_flag_1d: Id %d not found\n", id );
		return FALSE;		/* id not found */
	}

	if ( (PAWC.iq_eqv[HCBOOK.lcid+KBITS-1] & 0x1) == 0x1 ) {
		return TRUE;
	} else {
		return FALSE;
	}
}


bool
h_flag_2d(
	int		id
){
	HFIND(id,"h_flag_2d");

	if ( HCBOOK.lcid == 0 ) {
		sf_report( "h_flag_2d: Id %d not found\n", id );
		return FALSE;		/* id not found */
	}

	if ( (PAWC.iq_eqv[HCBOOK.lcid+KBITS-1] & 0x2) == 0x2 ) {
		return TRUE;
	} else {
		return FALSE;
	}
}


bool
h_flag_profile(
	int		id
){
	HFIND(id,"h_flag_profile");

	if ( HCBOOK.lcid == 0 ) {
		sf_report( "h_flag_profile: Id %d not found\n", id );
		return FALSE;		/* id not found */
	}

	if ( (PAWC.iq_eqv[HCBOOK.lcid+KBITS-1] & 0x80) == 0x80 ) {
		return TRUE;
	} else {
		return FALSE;
	}
}


char	h_rwn_chtitl[128];
int	h_rwn_nvar;
char	h_rwn_tags[MAX_RWN_COLS][9];
float	h_rwn_rlow[MAX_RWN_COLS];
float	h_rwn_rhigh[MAX_RWN_COLS];


bool
h_rwn_getInfo(
	int		idn
) {
	int		i;
	char		*p;

	h_rwn_nvar = MAX_RWN_COLS;
	HGIVEN(idn,h_rwn_chtitl,h_rwn_nvar,h_rwn_tags,h_rwn_rlow,h_rwn_rhigh);

	if ( h_rwn_nvar == 0 ) {
		return FALSE;
	}

	for( i=0 ; i < h_rwn_nvar ; i++ ) {
		for ( p = h_rwn_tags[i]; *p == ' ' ; p++ ) { }
		memmove( h_rwn_tags[i], p, 9 - ( p - h_rwn_tags[i] ) );
	}

	return TRUE;
}


int
h_rwn_getIndex(
	int		idn,
	char * const	name
) {
	int		nvar, i;

	if ( ! h_rwn_getInfo( idn ) ) {
		return -1;
	}

	for( i=0 ; i < h_rwn_nvar ; i++ ) {

		if ( strcasecmp( name, h_rwn_tags[i] ) == 0 ) {
			return i;
		}
	}

	return -1;
}


SMap
h_get_labels(
	int		id,
	char		*chopt
) {
	SMap	sm;
	int	i, j, n;
	char	lab[MAX_LABEL_LEN+1];

	qp_assert( !strcmp(chopt,"X") ||
		!strcmp(chopt,"Y") || !strcmp(chopt,"Z") );

	n = HLABNB(id,chopt);
	sm = smap_new( n );

	for ( i=0 ; i < n ; i++ ) {
		lab[0] = '\0';
		HLGNXT(id,i+1,lab,chopt);
		j = strlen( lab );
		if ( j < MAX_LABEL_LEN ) {
			memset( &lab[j], ' ', MAX_LABEL_LEN - j );
			lab[MAX_LABEL_LEN] = '\0';
		}
		smap_add( sm, lab );
	}

	return sm;
}


void
h_hbook1_labels(
	int		idh,
	char *		title,
	SMap		label_list
)
{
	int	n, i;
	char	clab[MAX_MAP_STRINGS][MAX_LABEL_LEN+1];

	for( i=0 ; i < MAX_MAP_STRINGS ; i++ ) {
		clab[i][0] = '\0';
	}

	n = smap_entries(label_list);
	HBOOK1( idh, title, n, 1., (float) (n+1), 0. );

	for ( i=0 ; i < n ; i++ ) {
		strncpy( clab[i], smap_get( label_list, i ), MAX_LABEL_LEN);
		clab[i][MAX_LABEL_LEN] = '\0';
	}
	HLABEL( idh, n, clab, "NX" );
}


void
h_hbook2_labels(
	int		idh,
	char *		title,
	SMap		labelx_list,
	SMap		labely_list,
	int		nbin[],
	float		min[],
	float		max[]
)
{
	int	n, i;
	char	clab[MAX_MAP_STRINGS][MAX_LABEL_LEN+1];

	for( i=0 ; i < MAX_MAP_STRINGS ; i++ ) {
		clab[i][0] = '\0';
	}

	if ( labely_list != 0 ) {
		nbin[0] = smap_entries(labely_list);
		min[0] = 1.;
		max[0] = nbin[0] + 1.;
	}

	if ( labelx_list != 0 ) {
		nbin[1] = smap_entries(labelx_list);
		min[1] = 1.;
		max[1] = nbin[1] + 1.;
	}

	HBOOK2( idh, title,
		nbin[1], min[1], max[1],
		nbin[0], min[0], max[0],
		0. );

	if ( labely_list != 0 ) {
		for ( i=0 ; i < nbin[0] ; i++ ) {
			strncpy( clab[i], smap_get( labely_list, i ),
				MAX_LABEL_LEN);
			clab[i][MAX_LABEL_LEN] = '\0';
		}
		HLABEL( idh, nbin[0], clab, "NY" );
	}

	if ( labelx_list != 0 ) {
		for ( i=0 ; i < nbin[1] ; i++ ) {
			strncpy( clab[i], smap_get( labelx_list, i ),
				MAX_LABEL_LEN);
			clab[i][MAX_LABEL_LEN] = '\0';
		}
		HLABEL( idh, nbin[1], clab, "NX" );
	}
}
