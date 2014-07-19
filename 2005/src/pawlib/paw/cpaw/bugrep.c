/*
 * $Id: bugrep.c,v 1.4 2005/04/18 15:23:12 mclareni Exp $
 *
 * $Log: bugrep.c,v $
 * Revision 1.4  2005/04/18 15:23:12  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.3  1997/03/14 14:06:44  mclareni
 * WNT mods
 *
 * Revision 1.2  1996/10/18 12:43:55  couet
 * - prototype for ku_edit
 * - prototype for ku_npar
 * - add a call to ku_vers
 *
 * Revision 1.1.1.1  1996/03/01 11:38:58  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
#if defined(CERNLIB_BUGREPORT)
/*CMZ :  2.07/19 21/11/95  18.17.13  by  Unknown*/
/*-- Author :    Maarten Ballintijn   24/08/94*/

/*
 *  bugreport.c  -  implementation of the bugreport command
 *
 */

/* arange external symbol ... */
#if defined(CERNLIB_QX_SC)
#define         bugreprt        bugreprt_
#endif
#if defined(CERNLIB_IBM)
#pragma         linkage(bugreprt,FORTRAN)
#endif
#if defined(CERNLIB_QXCAPT) || defined(CERNLIB_QFMSOFT)
#define         bugreprt  type_of_call BUGREPRT
#endif


#if !defined(CERNLIB_VAX)
#include        <sys/types.h>
#endif
#include        <time.h>
#include        <stdio.h>
#include        <assert.h>
#include        <string.h>
#include        <stdlib.h>
#define L_cuserid  9


char    *pavers();
char    *ku_vers();
char    *hbvers();
char    *hplvers();
char    *igvers();
char    *sivers();
#if 0
char    *csvers();
char    *mnvers();
#endif

char    *ku_expr( char * const s );
int     ku_edit( char *, int);
int     ku_npar();

/* changed from R_OK, which already exists in unistd.h and is used for
 * something else... */
#define MAIL_OK    0
#define MAIL_ERROR 1


static char     *br_uname_sysname;
static char     *br_uname_nodename;
static char     *br_uname_release;
static char     *br_uname_version;
static char     *br_uname_machine;

#if defined(__osf__)
#include        <malloc.h>
#endif

#include        <stdlib.h>


static char *
str_new( char * const s )
{
        char            *st;
        unsigned int    len;

        len = strlen( s ) + 1;
        st = calloc( 1, len ); assert( s );
        strcpy( st, s );

        return st;
}


#if defined(CERNLIB_UNIX)

#include        <sys/utsname.h>

static br_uname()
{
        struct utsname  un;

        if ( !br_uname_sysname ) {

                uname( &un );

                br_uname_sysname = str_new( un.sysname );
                br_uname_nodename = str_new( un.nodename );
                br_uname_release = str_new( un.release );
                br_uname_version = str_new( un.version );
                br_uname_machine = str_new( un.machine );
        }
}
#endif
#if defined(CERNLIB_VAX)

#include        <starlet.h>
#include        <descrip.h>
#include        <syidef.h>
#include        <lib$routines.h>

typedef struct _item_list_3_ {
        short int       buflen;
        short int       itemcode;
        char            *bufaddr;
        int             *retlen;
} item_list_3;

static char     un_hw_name[31];
static int      un_hw_name_len;
static char     un_node_swtype[5];
static int      un_node_swtype_len;
static char     un_node_swvers[5];
static int      un_node_swvers_len;
static char     un_nodename[16];
static int      un_nodename_len;
static char     un_version[9];
static int      un_version_len;

static item_list_3      itmlst[] = {
        31, SYI$_HW_NAME, un_hw_name, &un_hw_name_len,
        4, SYI$_NODE_SWTYPE, un_node_swtype, &un_node_swtype_len,
        4, SYI$_NODE_SWVERS, un_node_swvers, &un_node_swvers_len,
        15, SYI$_NODENAME, un_nodename, &un_nodename_len,
        8, SYI$_VERSION, un_version, &un_version_len,
        0, 0, 0, 0
};

static br_uname()
{
        int     r;

        if ( !br_uname_sysname ) {

                r = sys$getsyiw( 0, 0, 0, itmlst, 0, 0,  0);
                if ( !(r & 1 ) ) lib$signal( r );

                br_uname_sysname = str_new( un_node_swtype );
                br_uname_nodename = str_new( un_nodename );
                br_uname_release = str_new( un_node_swvers );
                br_uname_version = str_new( un_version );
                br_uname_machine = str_new( un_hw_name );
        }
}
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
static br_uname()
{
        if ( !br_uname_sysname ) {
                br_uname_sysname = "VM/CMS";
                br_uname_nodename = "Unknown";
                br_uname_release = "Unknown";
                br_uname_version = "Unknown";
                br_uname_machine = "IBM/370";
        }
}
#endif
#if defined(CERNLIB_WINNT)
static br_uname()
{
        if ( !br_uname_sysname ) {
                br_uname_sysname = "Windows/NT";
                br_uname_nodename = "Unknown";
                br_uname_release = "Unknown";
                br_uname_version = "Unknown";
                br_uname_machine = "Intel";
        }
}
#endif
/* template for user info, !! keep identical to br_mail_motif !! */
static char *br_tmpl_user[] = {
        "-----------------------------------------------------------",
        "* Please fill in the following fields *",
        "",
        "Name:",
        "Institute:",
        "Phone number:",
        "Email address:",
        "",
        0
};

#if defined(CERNLIB_IBM)
static
#endif
char *br_tmpl_bugrep[] = {
        "-----------------------------------------------------------",
        "* Give a short description of the bug *",
        "",
        "",
        "-----------------------------------------------------------",
        "* Describe how to repeat the problem *",
        "",
        "",
        "-----------------------------------------------------------",
        "* If known, suggest a way to fix *",
        "",
        "",
        "-----------------------------------------------------------",
        "Full version information will be automatically added below",
        "-----------------------------------------------------------",
        0
};


#if defined(CERNLIB_IBM)
static
#endif
char *br_tmpl_comment[] = {
        "-----------------------------------------------------------",
        "* Give your suggestion, comment or gripe *",
        "",
        "",
        "",
        0
};

#if defined(CERNLIB_UNIX) && !defined(CERNLIB_WINNT) && !defined(CERNLIB_VAX)
#include <unistd.h>
#endif

/* new function to create a temporary file without having a race condition
 * (currently only on UNIX) */
static char *
br_create_tempfile(void)
{
  static char * result = 0;
#if defined(CERNLIB_UNIX) && !defined(CERNLIB_WINNT) && !defined(CERNLIB_VAX)
  int fd;
  char *tempdir = getenv("TMPDIR");
  char filename[] = "/bugrepXXXXXX";
  
  if ( tempdir && *tempdir ) {
    result = (char *)calloc( strlen( tempdir ) + strlen( filename ) + 1, 1 );
    sprintf( result, "%s%s", tempdir, filename );
  }
  else {
    result = (char *)calloc( strlen( "/tmp" ) + strlen( filename ) + 1, 1 );
    sprintf( result, "/tmp%s", filename );
  }
  
  if ( (fd = mkstemp(result)) < 0 ) {
    /* no need to print an error here as it will be done by other functions */
    free( result );
    result = 0;
  }
  else
    /* don't want to leave open file descriptors about.
     * unlike tmpfile(), mkstemp() doesn't delete the file automatically
     * when no one is using it, so it's ok to do this */
    close( fd );

#else
  /* don't know if mkstemp() exists on non-Unix operating systems, and in
   * any case the above code won't work because directory separators, etc.
   * are different (at least on Windows and VMS) */
  result = (char *)calloc( L_tmpnam, 1 );
  if ( ! tmpnam( result ) /* INSECURE!!! */ ) {
    free( result );
    result = 0;
  }
    
#endif /* defined(CERNLIB_UNIX) */

  /* note that the file with filename "result" must be remove()d and the
   * string "result" free()d by the calling function */
  return result;
}

static int
br_create_template( char *tmp_file, char mode )
{
        FILE    *fp;
        char    **s;

        if ( tmp_file == 0 || *tmp_file == 0 ) {
                /* report error */
                printf( "BUGREPORT: cannot get temporary file\n" );
                return MAIL_ERROR;
        }

        fp = fopen( tmp_file, "w" );
        if ( fp == NULL ) {
                printf( "BUGREPORT: cannot open temporary file\n" );
                return MAIL_ERROR;
        }

        for( s = br_tmpl_user; *s ; s++ ) {
                fputs( *s, fp );
                fputc( '\n', fp );
        }

        if ( mode == 'B' )      /* select a template */
                s = br_tmpl_bugrep;
        else
                s = br_tmpl_comment;

        for( ; *s ; s++ ) {
                fputs( *s, fp );
                fputc( '\n', fp );
        }

        fclose( fp );

        return MAIL_OK;
}


static int
br_edit_file( int first, char * tmp_file )
{
        int     r, ist;
        char    * buf;
	
	if ( tmp_file == 0 || *tmp_file == 0 )
	  return MAIL_ERROR;

        /* suppress the adding of .kumac by prepending a - ... */
	buf = (char *)malloc( strlen(tmp_file) + 2 );
        buf[0] = '-';
        strcpy( &buf[1], tmp_file );

        ist = ku_edit( buf, 0 );
        switch( ist ) {
        case 0: /* all ok */
                r = MAIL_OK;
                break;

        case 1: /* user abort */
                if ( first ) {
                        printf("BUGREPORT: user abort of editing bugreport\n");
                        r = MAIL_ERROR;
                } else {
                        r = MAIL_OK;       /* second edit needs not to modify */
                }
                break;

        case 2: /* error */
                printf( "BUGREPORT: problem editing bugreport\n" );
                r = MAIL_ERROR;
                break;

        default:
                printf( "BUGREPORT: ku_edit returned %d ??\n", ist );
                r = MAIL_OK;
                break;
        }

	free( buf );
        return r;
}


static int
br_add_config( char * tmp_file )
{
        FILE    *fp;
        char    *k_OS;
        char    *k_MACHINE;
        char    *s, *buginfo();

	if ( tmp_file == 0 || *tmp_file == 0 ) {
              printf( "BUGREPORT: problem adding version info to bugreport\n" );
	      return MAIL_ERROR;
	}
	
        fp = fopen( tmp_file, "a" );
        if ( fp == NULL ) {
              printf( "BUGREPORT: problem adding version info to bugreport\n" );
              return MAIL_ERROR;
        }

        s = buginfo();

        fprintf( fp, "%s\n", pavers() );
        fprintf( fp, "%s\n", ku_vers() );
        fprintf( fp, "%s\n", hbvers() );
        fprintf( fp, "%s\n", hplvers() );
        fprintf( fp, "%s\n", igvers() );
        fprintf( fp, "%s\n", sivers() );

        br_uname();     /* get "uname" fields */

        k_OS = ku_expr( "$OS" );
        k_MACHINE = ku_expr( "$MACHINE" );

        fprintf( fp, "Kuip info:        $OS=%s, $MACHINE=%s\n",
                k_OS, k_MACHINE );

        fprintf( fp, "Operating System: %s %s\n",
                br_uname_sysname, br_uname_release );
        fprintf( fp, "Node name:        %s\n",
                br_uname_nodename );
        fprintf( fp, "Hardware type:    %s\n",
                br_uname_machine );

        fclose( fp );
        free( k_OS );
        free( k_MACHINE );

        return MAIL_OK;
}


static int
br_mail_file( char *addr, char *subj, char *file )
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))
{
        FILE            *mfp, *fp;
        char            line[1024], my_addr[L_cuserid];
        time_t          clock;
        struct tm       *ts;

        fp = fopen( file, "r" );
        if ( fp == NULL ) {
                printf( "BUGREPORT: cannot (re)open temporary file\n" );
                return MAIL_ERROR;
        }

        cuserid( my_addr );

        sprintf( line, "/usr/lib/sendmail -t" );
        mfp = popen( line, "w" );

        if ( mfp == NULL ) {
                printf( "BUGREPORT: cannot run sendmail\n" );
                return MAIL_ERROR;
        }


        clock = time( (time_t *) 0 );
        ts = localtime( &clock );
        strftime( line, sizeof( line ),
                "%a, %d %h %y %H:%M:%S", ts );
        fprintf ( mfp, "To: %s\n", addr );
        fprintf ( mfp, "Bcc: %s\n", my_addr );
        fprintf ( mfp, "Date: %s\n", line );
        fprintf ( mfp, "Subject: %s\n", subj );
        fprintf ( mfp, "\n" );

        while( fgets( line, sizeof( line ), fp ) != NULL ) {
                fputs( line, mfp );
        }
        fclose( fp );

        pclose( mfp );

        return MAIL_OK;
}
#endif
#if defined(CERNLIB_VAX)
{
        char    line[1024];

        sprintf( line, "mail/self/subject=\"%s\" %s. dxmint::\"\"\"%s\"\"\"",
                subj, file, addr );

        system( line );

        return MAIL_OK;
}
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
{
        char    line[1024], *p;
        int     rc;

        if ( p = strchr( file, '*' ) ) {
                *p = ' ';
        }

        sprintf( line, "exec mail %s ( noedit noprompt file %s subject %s\n",
          addr, file, subj );

        rc = system(line);

        return rc == 0 ? MAIL_OK : MAIL_ERROR;
}
#endif

void
bugreprt()
{
        char    *tmp_file, *s, *ku_proc(), *ku_getc(), mode, *addr;
        int     r, first;

        r = MAIL_OK;

        /* determine mode B or C */

        switch( ku_npar() ) {
        case 0: /* send bugrep */
                mode = 'B';
                break;
        case 1: /* check */
                s = ku_getc();
                if ( *s == 'B' || *s == 'C' ) {
                        mode = *s;
                } else {
                        printf( "BUGREPORT: Illegal option(s).\n" );
                        r = MAIL_ERROR;
                }
                break;
        default: /* huh ? */
                printf( "BUGREPORT: Illegal option(s).\n" );
                r = MAIL_ERROR;
                break;
        }

        /* create temp file */

        if ( r == MAIL_OK ) {
                tmp_file = br_create_tempfile();
                r = br_create_template( tmp_file, mode );
	}

        /* have the user edit the temp file */

        for( first = 1 ; ; ) {

                if ( (r == MAIL_OK) && (first || (strcmp( "EDIT", s ) == 0)) ) {
                        r = br_edit_file( first, tmp_file );
                        first = 0;
                        if ( r == MAIL_OK ) {
                                s =
ku_proc( "Type 'send' to submit, 'edit' to re-edit or 'abort' to cancel: ",
NULL );
                                continue;
                        } else {
                                break;
                        }
                } else if ( strcmp( "SEND", s ) == 0 ) {
                        r = MAIL_OK;
                        break;
                } else if ( strcmp( "ABORT", s ) == 0 ) {
                        r = MAIL_ERROR;
                        break;
                } else {
                        s =
ku_proc( "Please type 'send', 'edit' or 'abort' (without quotes): ", NULL );
                }
        }

        /* add config info */

        if ( r == MAIL_OK )
                r = br_add_config( tmp_file );

        /* send the temp file */

        if ( r == MAIL_OK ) {
                s = mode == 'B' ? "PAW Bug Report" : "PAW Comment";

                addr = getenv( "PAWSUPPORT" );
                if ( addr == 0 ) {
                        addr = "Paw.Support@cern.ch";
                }
                r = br_mail_file( addr, s , tmp_file );
        }

        if ( r == MAIL_OK ) {
                printf( "BUGREPORT: Report sent\n" );
        } else {
                printf( "BUGREPORT: No report sent\n" );
        }

        /* clean up */

	if ( tmp_file && *tmp_file )
	  remove( tmp_file );
	free( tmp_file );
}


static char     *br_info_str;


char *
buginfo()
{
        char    buf[256];

        if ( !br_info_str ) {
                br_info_str = malloc( 2048 );

                sprintf( buf, "%s\n", pavers() );
                strcpy( br_info_str, buf );

                br_uname();     /* get "uname" fields */

                sprintf( buf, "Operating System: %s %s\n",
                        br_uname_sysname, br_uname_release );
                strcat( br_info_str, buf );

                sprintf( buf, "Hostname:         %s\n",
                        br_uname_nodename );
                strcat( br_info_str, buf );

                sprintf( buf, "Hardware type:    %s\n",
                        br_uname_machine );
                strcat( br_info_str, buf );

                br_info_str = realloc( br_info_str, strlen(br_info_str) + 1 );

        }

        return br_info_str;
}


#if defined(CERNLIB_IBM)
static
#endif
int
br_mail_motif( char mode, char *name, char *inst, char *phone, char *email,
             char *message)
{
        char    *tmp_file, **s, *st, *addr;
        FILE    *fp;
        int     r;

        tmp_file = br_create_tempfile();
        if ( tmp_file == 0 || *tmp_file == 0 ) {
                /* report error */
                printf( "BUGREPORT: cannot get temporary file\n" );
		free( tmp_file );
                return MAIL_ERROR;
        }

        fp = fopen( tmp_file, "w" );
        if ( fp == NULL ) {
                printf( "BUGREPORT: cannot open temporary file\n" );
		free( tmp_file );
                return MAIL_ERROR;
        }

        for( s = br_tmpl_user; *s ; s++ ) {
                if ( strcmp( "Name:", *s ) == 0 ) {
                        fprintf( fp, "%-16s%s\n", *s, name );
                } else if ( strcmp( "Institute:", *s ) == 0 ) {
                        fprintf( fp, "%-16s%s\n", *s, inst );
                } else if ( strcmp( "Phone number:", *s ) == 0 ) {
                        fprintf( fp, "%-16s%s\n", *s, phone );
                } else if ( strcmp( "Email address:", *s ) == 0 ) {
                        fprintf( fp, "%-16s%s\n", *s, email );
                } else {
                        fputs( *s, fp );
                        fputc( '\n', fp );
                }
        }

        fprintf( fp, "%s", message );

        fclose( fp );

        r = br_add_config( tmp_file );

        if ( r == MAIL_OK ) {
                st = mode == 'B' ? "PAW Bug Report" : "PAW Comment";

                addr = getenv( "PAWSUPPORT" );
                if ( addr == 0 ) {
                        addr = "Paw.Support@cern.ch";
                }
                r = br_mail_file( addr, st , tmp_file );

                if ( r == MAIL_OK )
                        printf( "BUGREPORT: Report sent\n" );
        }

	/* clean up */
	if ( tmp_file && *tmp_file )
	  remove( tmp_file );
	free( tmp_file );
	
        return r;
}
#endif
