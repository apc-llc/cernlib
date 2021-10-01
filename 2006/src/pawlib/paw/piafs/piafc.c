/*
 * $Id: piafc.c,v 1.13 1999/11/15 13:08:21 couet Exp $
 *
 * $Log: piafc.c,v $
 * Revision 1.13  1999/11/15 13:08:21  couet
 * - was including cfortran.h from the wrong place
 *
 * Revision 1.12  1998/08/25 12:48:33  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.11  1997/03/24 09:22:16  dinofm
 * A blank character is appended at the end of the command executed in the
 * Korn shell (this makes pwd work again ?!?).
 *
 * Revision 1.10  1997/03/05 14:41:16  dinofm
 * Use ulog.user instead than real_user_name when staging files.
 *
 * Revision 1.9  1997/02/14 16:17:31  dinofm
 * Message of the day is always returned (with CS2 motd as well).
 * New setup for NFSCACHE on Meiko (used to hang on on Solaris 2.5.1).
 * Some more information put in logs to catch PSMPUT timeouts.
 * System commands are executed in Korn shell on CS2.
 *
 * Revision 1.8  1996/12/12 14:05:29  dinofm
 * A couple of #ifdef have been added to avoid calling the stager functions
 * when no stager is used.
 * The invokation of umlog_user was removed (umlog has been dropped).
 *
 * Revision 1.7  1996/12/10 11:12:53  dinofm
 * A new function to check user disk quota has been added. The actual
 * code is embedded in a script that is run on the PIAF side.
 *
 * Revision 1.6  1996/12/04 17:01:54  dinofm
 * I added support for the new SHIFT STAGER on the CS2. Such a new stager is
 * now the default.
 *
 * Revision 1.5  1996/06/12 09:42:19  dinofm
 * Management of ntuple cache allocation.
 * DPM subdirectories bug was fixed (now files with the same name can
 * co-exist in different Piaf subdirectories).
 *
 * Revision 1.4  1996/04/25 07:53:03  dinofm
 * VAX/VMS code wiped out.
 *
 * Revision 1.3  1996/04/24 08:39:57  dinofm
 * Functions to manage graphical cut 'unpacking' and further decoding of
 * PIAF/MESSAGE command have been added.
 * The user's working directory can now be changed by adding the 'workdir'
 * keyword to piaf.conf.
 *
 * Revision 1.2  1996/04/02 21:49:38  thakulin
 * Rename SOLARIS define to CERNLIB_SOLARIS.  Include piaf.h from this
 * directory.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :          31/01/96  11.29.21  by  Timo Hakulinen*/
/*-- Author :    Alfred Nathaniel   07/04/93*/

/* Maximum number of open fd's, used by the FD_SET macro's */
/* on HP set in types.h on AIX in select.h */
#define FD_SETSIZE  200

#include <sys/types.h>
#ifdef sun
#define BSD_COMP /* to get BSD ioctl's in Solaris */
#endif
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/time.h>
#include <errno.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <limits.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#ifdef __hpux
#include <symlink.h>
#endif
#ifdef _AIX
#include <time.h>
#include <sys/select.h>
#endif

#if defined(__hpux) || defined(_AIX) || defined(__osf__)
#define FDT  (int *)
#else
#define FDT  (fd_set *)
#endif

#if defined(__hpux) || defined(_AIX) || defined(__osf__) || \
    defined(__sgi) || defined(sun)
#define HAVE_RSTAT
#endif

#if defined(sun) && defined(CERNLIB_SOLARIS)
/* if Meiko CS-2, this file should contain symbol EF_MEIKO_ELAN1 */
# include <sys/elf.h>
  /* # ifdef EF_MEIKO_ELAN1 */
#  define MEIKO_CS2
  /* # endif */
#endif

#ifdef HAVE_RSTAT
#include <rpcsvc/rstat.h>
#endif


#include "piaf.h"


#ifndef PATH_MAX
#  define PATH_MAX  512
#endif

#define FD_SETSIZE  200      /* Maximum number of open fd's */
#define MAX_SLAVES   32      /* Maximum number of slave servers */
#define TIMEOUT      20      /* Timeout of 20 seconds */
#define MAX_TIMEOUT   3      /* Maximum number of times we may timeout */
#define MAX_STAGER    8      /* Maximum number of master stagers */

/* Timo's stager is replaced by default with the new STAGER SW */

#if defined(CERNLIB_SUN)
#  if defined(OLD_STAGER)
#    define PFGET
#  else
#    define STAGEALLOC
#    define STDPOOL  "piafstage"
#    define NA49POOL "na49stage"
#    define NA49GRP  "vp"
#    define STAGECONFIG "/etc/shift.conf"
#    define STGKEYW   "STG"
#    define HOSTKEYW  "HOST"
#    define STGKEYWL  3
#    define HOSTKEYWL 4
#  endif
#endif

/* The script that checks user quotas */
#define QUOTER      "bin/quoter"

/*
 * A file is a Hbook file when we find a hbook_key within KEY_LIMIT bytes.
 * The hbook_key resides in the fist few hundred bytes of the second record.
 * The current length allows us to find a key in files with a record length
 * of 32768 words (163840 = 4 * 32768 + 32768).
 */
#define KEY_LIMIT   163840

/*
 * Since the minimum reclen is 8192, we need at least 20 buffers to store
 * before KEY_LIMIT is reached (22 for safety).
 */
#define MAX_BUF     22

/* New stuff to deal with the new QP & other enhancements */
#include "ntuple/qp_cuts.h"
#include <cfortran/cfortran.h>
#include "ntuple/errors.h"
#define PQRY_KEYW	"PQRY"
#define INVALID_KEYW	"Invalid"
#define VERS_KEYW	"VERS"
#define DEFAULT_VMEM_SIZE "54"

/* exported external routines */

#define PSDVRT   psdvrt_
#define PSPLOG   psplog_
#define PSMAIN   psmain_
#define PSEXIT   psexit_
#define PSHOST   pshost_
#define PSSCAN   psscan_
#define PSTIME   pstime_
#define PSSTAT   psstat_
#define PSGETA   psgeta_
#define PSPUTA   psputa_
#define PSMPUT   psmput_
#define PSMBUF   psmbuf_
#define PSMSEL   psmsel_
#define PSADDR   psaddr_
#define PSCPU    pscpu_
#define PSRM     psrm_
#define PSCP     pscp_
#define PSMV     psmv_
#define PSINTR   psintr_
#define PSALARM  psalarm_
#define PSSYSTEM pssystem_

/* prototypes needed because routines are called also from inside this file */
SUBROUTINE PSCP(char *, int);
SUBROUTINE PSRM(char *, int);


/* imported external FORTRAN routines */

#define           CZPUTA czputa_
extern SUBROUTINE CZPUTA( char*, INTEGER*, int );
#define      CALL_CZPUTA(line,status) CZPUTA(line,status,strlen(line))

#define           CZPUTC czputc_
extern SUBROUTINE CZPUTC( INTEGER*, INTEGER* );

#define           FZOUT  fzout_
extern SUBROUTINE FZOUT( INTEGER*, INTEGER*, INTEGER*, INTEGER*,
                        char*, INTEGER*, INTEGER*, INTEGER*, int );
#define      CALL_FZOUT(lun,ixdiv,lentry,ievent,chopt,iox,nuh,iuhead) \
                     do { \
                       INTEGER LUN = lun; \
                       INTEGER IXDIV = ixdiv; \
                       INTEGER LENTRY = lentry; \
                       INTEGER IEVENT = ievent; \
                       INTEGER IOX = iox; \
                       INTEGER NUH = nuh; \
                       FZOUT( &LUN, &IXDIV, &LENTRY, &IEVENT, \
                             chopt, &IOX, &NUH, iuhead, strlen(chopt) ); \
                     } while( 0 )


#define           PSEXEC psexec_

extern SUBROUTINE PSEXEC( INTEGER* );

#define           HBVM hbvm_
extern SUBROUTINE HBVM( REAL* );

#define           HBGMAX hbgmax_
extern SUBROUTINE HBGMAX( INTEGER* );

#define           HBFREE hbfree_
extern SUBROUTINE HBFREE( INTEGER* );


/* imported external routines from KUIP */
extern char *strupper( char* );
extern int  ku_stop( int );
extern void km_file_type(char *, char *, char *, int);
extern char *km_strip(char *);
extern char **km_scan_dir(char *, char *, int, int);
extern char *fexpand(const char *, const char *);
extern char *ku_qexe(char *);


/* from TCPIAF */
extern int  recv_buf( int, char*, int );
extern int  recv_len( int, char*, int );
extern int  send_buf( int, char*, int );
extern int  send_eof( int );
extern int  send_str( int, char* );
extern int  send_len( int, char*, int );
extern void tcp_nodelay( int, int );
extern void tcp_setbufs( int );
extern void piaf_sync( int, char* );
extern void set_nonblock(int);
extern void clr_nonblock(int);
extern int oob_timeout(int *);


/* from pstage.c */
extern void stginit(char *,int,char *,char *,char *);
extern void stginfo(void);


/* local functions */
static void cleanup(void);
static void fatalperror(char *msg);
static void fatal(char *msg);
static void signal_handler(int);

#ifdef STAGEALLOC
char *GetPool(void);
char *GetStgHost(void);
char *SetStgHost(void);
#endif

/* local data */
static char *CZBUFC;              /* buffer for CZPUTC */
static FILE *login;               /* read from log file */

static int   server_protocol;
static char  server_version[16];
static char  server_host[64];
static char  client_host[64];
static char  stage_host [64];
static char  real_user_name[64];
static char  conf_dir[64];        /* directory containing PIAF config files */
static char  homedir[PATH_MAX+1]; /* user's home directory */
static char  workdir[PATH_MAX+1]; /* user's work directory */
static char  logfile[64];         /* file used to catch stdout */

static int   master;              /* piaf master server else slave server */
static int  *slave_isocket;
static char *slave_hosts;
int         *active_slaves;
int         *running_slaves;

static sigjmp_buf stack_env;
static int hangup;
static int isocket;
static int osocket;

static clock_t rtime0;            /* real time reference */

static char debuf[80];
static char VmemSize[80];

/* global used by libPW.a */
int Error;

/* dummy routine used in //KUIP/CMUTIL/KMFILE */
char *km_get_br_filter(void){ return NULL; }


/***********************************************************************
 *                                                                     *
 * Print Message Of The Day.                                           *
 *                                                                     *
 ***********************************************************************/
static
int cat_motd( const char* version )
{
  char motdname[128];
  char lastname[PATH_MAX+1];
  time_t modtime;
  struct stat statbuf;
  FILE *motd;
  int fd;
  int show = 0;

  /* if file nopiaf exists print its contents and close connection */
  sprintf( motdname, "%s/etc/nopiaf", conf_dir );
  if( (motd = fopen( motdname, "r" )) != NULL ) {
    int c;
    printf( "\n" );
    while( (c = getc( motd )) != EOF ) {
      putchar( c );
    }
    fclose( motd );
    printf( "\n" );

    return -1;
  }

  /* get last modification time of the file ~/.piaflast */
  sprintf(lastname, "%s/.piaflast", homedir);
  if (stat(lastname, &statbuf) == - 1)
     modtime = 0;
  else
     modtime = statbuf.st_mtime;

  /* show motd at least once per day 
  if (time(NULL) - modtime > (time_t)86400) */
  /* show motd ALWAYS */
     show = 1;

  /* general messages */
  sprintf( motdname, "%s/etc/motd", conf_dir );
  if (stat(motdname, &statbuf) == 0) {
     if (statbuf.st_mtime > modtime || show) {
        if ((motd = fopen( motdname, "r" )) != NULL) {
           int c;
           printf( "\n" );
           while ((c = getc( motd )) != EOF) {
              putchar( c );
           }
           fclose( motd );
           printf( "\n" );
        }
	/* Get the general CS2 motd as well */
        if ((motd = fopen( "/etc/motd", "r" )) != NULL) {
           int c;
           printf( "\n" );
           while ((c = getc( motd )) != EOF) {
              putchar( c );
           }
           fclose( motd );
           printf( "\n" );
        }
     }
  }

  if (version != NULL) {
     /* version specific messages */
     strcat( motdname, version );
     if (stat(motdname, &statbuf) == 0) {
        if (statbuf.st_mtime > modtime || show) {
           if ((motd = fopen( motdname, "r" )) != NULL) {
              int c;
              while ((c = getc( motd )) != EOF) {
                 putchar( c );
              }
              fclose( motd );
              printf( "\n" );
           }
        }
     }
  }

  fd = creat(lastname, 0600);
  close(fd);

  return 0;
}

/***********************************************************************
 *                                                                     *
 * Redirect the socket output to a file descriptor different           *
 * from stdout.                                                        *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSDVRT(INTEGER *isock, INTEGER *type, char *chbuf, char *chuser,
                  char *chvers, char *condir, INTEGER *iproto,
                  INTEGER *iwk, INTEGER *islvid,
                  int len_chbuf, int len_chuser, int len_chvers,
                  int len_condir)
{
  int    istat, ret;
  int    sock = *isock;
  int    newdir = 0;
  struct stat statbuf;
  static char homeenv[PATH_MAX+6]; /* must be static, putenv only stores ptr*/
  static char pathenv[PATH_MAX+6]; /* idem */
  char conffile[256];
  FILE *piafconf;
  struct sockaddr_in peersock;
  int    peersize = (sizeof peersock);
  struct hostent *peerhost;
  struct passwd *pwent = getpwuid(getuid());
  struct group *grpent = getgrgid(getgid());
  char   group_name[64];
  char   umon_host_name[64];
  char   *stg_mip, *stg_cip;
  int    stg_port;
  char  *p;
  struct tms tmsbuf;

  master = *type;
  rtime0 = times( &tmsbuf );    /* initialize connection time counter */

  CZBUFC = chbuf;

  /* store user name on the client system */
  strncpy( real_user_name, chuser, len_chuser );
  p = strchr( real_user_name, ' ' );
  *p = '\0';
  if (strlen(real_user_name) == 0)
     strcpy(real_user_name, "??");

  /* store server version number */
  strncpy( server_version, chvers, len_chvers );
  p = strchr( server_version, ' ' );
  *p = '\0';

  /* store PIAF system directory, usually /usr/piaf */
  strncpy( conf_dir, condir, len_condir );
  p = strchr( conf_dir, ' ' );
  *p = '\0';

  /* store protocol level */
  server_protocol = *iproto;

  /* fill in some fields of the user log structure */

  if (pwent != NULL && pwent->pw_name != NULL)
     strcpy(ulog.user, pwent->pw_name);
  else
     strcpy(ulog.user, "*****");

  if (grpent != NULL && grpent->gr_name != NULL)
     strcpy(ulog.group, grpent->gr_name);
  else
     strcpy(ulog.group, "**");

  strcpy(ulog.remote_user, real_user_name);

  if ((ret = getpeername(sock,(struct sockaddr *)&peersock,&peersize)) != -1
      && (peerhost = gethostbyaddr((char*)&peersock.sin_addr.s_addr,
                                   sizeof(struct in_addr),
                                   AF_INET )) != NULL ) {
     stg_cip = strdup(inet_ntoa(peersock.sin_addr));
     strcpy(ulog.remote_host, peerhost->h_name);
  } else if (ret != -1) {
     stg_cip = strdup(inet_ntoa(peersock.sin_addr));
     sprintf(ulog.remote_host,"[%s]",stg_cip);
  } else {
     stg_cip = (char *)0;
     strcpy(ulog.remote_host, "unknown");
  }

  /*
   * Initialize static stager data
   */
  if (master) {
    char node_ip[MAX_STAGER][32];
    int node_port[MAX_STAGER];
    int n, nnodes = 0;

    sprintf( conffile, "%s/etc/piaf.conf", conf_dir );
    stg_mip = (char *)0;
    if( (piafconf = fopen( conffile, "r" )) != NULL ) {
      char line[256];

      while( fgets( line, (sizeof line), piafconf ) != NULL ) {
        char word[4][64];
        int nword = sscanf( line, " %s %s %s %s",
                           word[0], word[1], word[2], word[3] );
        /*
         * all nodes where the stager daemon is running
         * must be configured by a line
         *     stager <name> <port>
         */
        if( nword >= 3 && strcmp( word[0], "stager" ) == 0 ) {
          struct hostent *hp;

          if( (hp = gethostbyname( word[1] )) != NULL ) {
            strcpy(node_ip[nnodes],inet_ntoa(*(struct in_addr *)hp->h_addr));
            node_port[nnodes] = atoi(word[2]);
            nnodes++;
          }
        }
      }
      fclose(piafconf);
    }
    if (nnodes > 0) {
      /* assign one of the available masters at random */
      srand((unsigned int)time((time_t)0));
      n = rand() % nnodes;
      stg_mip = strdup(node_ip[n]);
      stg_port = node_port[n];
      stginit(stg_mip,stg_port,stg_cip,ulog.remote_host,
        ulog.remote_user[0] == '?' ? (char *)0 : ulog.remote_user);
    }
  }

  strcpy(ulog.master_node, server_host);
  strcpy(ulog.version, server_version);
  ulog.protocol = server_protocol;
  ulog.workstation_type = *iwk;

  umask( S_IWGRP | S_IWOTH );   /* deny write access for group and world */

  piafl_open(master);

  /* Read loop over the configuration file to retrieve new parameters.   */
  /* (e.g. workdir, PAW's VMEM size)                                     */
  /* Change workdir. If nothing is found here then apply the default     */
  /* rule ($HOME/piaf). */

  /* Initialisations */
  workdir[0] = '\0';
  strcpy (VmemSize, DEFAULT_VMEM_SIZE);

  sprintf( conffile, "%s/etc/piaf.conf", conf_dir );
  if( (piafconf = fopen( conffile, "r" )) != NULL ) {
    char line[256];
    while( fgets( line, (sizeof line), piafconf ) != NULL ) {
      char word[4][64];
      int nword;
      nword = sscanf( line, " %s %s %s %s",word[0],word[1],word[2],word[3] );
      if(( nword >= 2) && (strcmp( word[0], "workdir" ) == 0) ) {
	/* We expect the value read from config file being something like
	 * dir/subdir/%s  where dir & subdir are directories and %s stands for
	 * the user name (e.g. /aleph/piaf/%s )
	 */
        sprintf (workdir,word[1],ulog.user);
      }
      if(( nword >= 2) && (strcmp( word[0], "vmem" ) == 0) ) {
        strcpy (VmemSize,word[1]);
      }
    }
    fclose(piafconf);
  }

  if (strlen(workdir) == 0) {
    /* Default: piafront did set the cwd to the home directory, just get it */
    getcwd( homedir, PATH_MAX );
    /* The workdir will be $HOME/piaf */
    sprintf( workdir, "%s/piaf", homedir );
  }

  /*
   * Set $HOME to the login directory.
   * Then //piaf/~/file.hbook will refer to a file in the home directory
   * and //piaf/file.hbook refers to a file in the user's working directory.
   */
  sprintf( homeenv, "HOME=%s", homedir );
  putenv( homeenv );

  sprintf( pathenv, "PATH=/bin:/usr/bin:/usr/contrib/bin:/usr/local/bin" );
  putenv( pathenv );

  /* check if the working directory already exists, if not create it */
  if( stat( workdir, &statbuf ) == - 1 ) {
    if( errno == ENOENT ) {
      /* does not exist */
      newdir = 1;
    }
    else {
      piafl_error("Couldn't stat workdir: %m");
      fatalperror("Can't stat workdir");
    }
  }
  else if( !S_ISDIR( statbuf.st_mode ) ) {
    /* exists but is not a directory */
    remove( workdir );
    newdir = 1;
  }

  /* create a working directory and cd to it */
  if( newdir && mkdir( workdir, 0755 ) < 0 ) {
     piafl_error("Couldn't create workdir: %m");
     fatalperror("Can't create workdir");
  } else if (!newdir && chmod(workdir, 0755) < 0) {
     piafl_error("Couldn't chmod workdir: %m");
     fatalperror("Can't chmod workdir");
  }

  if ((chdir(workdir)) < 0) {
     cleanup();
     piafl_error("Couldn't cd to workdir: %m");
     fatalperror("Can't cd to workdir");
  }

  if ((*isock = dup(sock)) < 0) {
     cleanup();
     piafl_error("Couldn't duplicate output socket: %m");
     fatalperror("Can't duplicate output socket");
  }

  /* remove all previous log files and create a new log file */
  if (master) {
     system( "/bin/rm -f .piaf*.log" );
     sprintf(logfile, ".piaf_%d.log", getpid());
  } else
     sprintf(logfile, ".piafs%d_%d.log", *islvid, getpid());

  if ((freopen(logfile, "w", stdout)) == NULL) {
     cleanup();
     piafl_error("Couldn't freopen stdout: %m");
     fatalperror("Can't freopen stdout");
  }
  if ((dup2(fileno(stdout), fileno(stderr))) < 0) {
     cleanup();
     piafl_error("Couldn't redirect stderr: %m");
     fatalperror("Can't redirect stderr");
  }

  if ((login = fopen(logfile, "r")) == NULL) {
     cleanup();
     piafl_error("Couldn't open logfile: %m");
     fatalperror("Can't open logfile");
  }

  /* construct client host and user name for umlog */
  strcpy(client_host, ulog.remote_host);

  strupper(strcpy(group_name, ulog.group));

  strcpy( umon_host_name, client_host );
  if( (int)strlen( umon_host_name ) < 6 )
    strcat( umon_host_name, "......" );

  if( strchr( client_host, '.' ) != NULL
     && strstr( client_host, ".cern.ch" ) == NULL ) {
    umon_host_name[5] = '*';    /* flag external connections */
  }
  strcpy( umon_host_name + 6, strupper( group_name ) );

  if (master) {
     if ((cat_motd( server_version )) == -1) {
        *islvid = -1;
        return;
     }
  }
}

/***********************************************************************
 *                                                                     *
 * Read output from log file and send it to client.                    *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSPLOG( INTEGER *iquest, INTEGER *istat )
{
  off_t ltot;
  off_t lnow;
  int   left;
  INTEGER iqsave[100];

  /* save IQUEST vector */
  if( iquest != NULL )
    memcpy( iqsave, iquest, (sizeof iqsave) );

  /* Determine the number of bytes left to be read from log file */
  fflush( stdout );
  ltot = lseek( fileno( stdout ), (off_t)0, SEEK_END );
  lnow = lseek( fileno( login ), (off_t)0, SEEK_CUR );
  left = ltot - lnow;

  if( left <= 0 ) {
    CALL_CZPUTA( "HLOGPF EMPTY", istat );
  }
  else {
    CALL_CZPUTA( "HLOGPF", istat );

    while( left > 0 ) {
      char line[256];
      INTEGER ntot;
      int n;

      if (fgets(line, (sizeof line), login) == NULL) {
         n = 0;
         left = 0;
         line[0] = '\0';
      } else {
         n = strlen( line );
         left -= n;
         line[--n] = '\0';         /* remove \n */
      }

      while( n > 0 && line[n-1] == ' ' )
        line[--n] = '\0';       /* remove trailing blanks */

      /*
       * send line preceded by length; length<0 indicates last line
       */
      ntot = n + 6;
      if( left == 0 )
        n = -(n + 1);
      sprintf( CZBUFC, "%6d%s", n, line );
      CZPUTC( &ntot, istat );
    }
  }

  CALL_FZOUT(998,0,0,1,"Z",2,100,iqsave);
  if( iquest != NULL )
    *istat = iquest[0];
  else
    *istat = 0;
}

/***********************************************************************
 *                                                                     *
 * Signal handler.                                                     *
 *                                                                     *
 ***********************************************************************/
static void signal_handler( int sig )
{
  char *msg = NULL;
  char umsg[32];
  char oob_byte;
  int  nbyte;
  char waste[BUFSIZ];
  int  blocked = 0;
  int  in_use = -1;

  hangup = 0;

  switch( sig ) {

  case SIGURG:
    /* receive the OOB byte */
    while( recv( isocket, &oob_byte, 1, MSG_OOB ) < 0 ) {

      if( errno == EWOULDBLOCK ) {

        /*
         * In some systems (Solaris) regular recv() does not return upon
         * receiving the oob byte, which makes the below call to recv()
         * block indefinitely if there are no other data in the queue.
         * Use non-blocking mode to see if there is anything to flush.
         * We used to use FIONREAD ioctl, but it caused other problems
         * due to kernel bugs.  Besides non-blocking I/O is POSIX whereas
         * FIONREAD is just old 4.3BSD.
         */
        set_nonblock(isocket);
        nbyte = recv( isocket, waste, (sizeof waste), 0 );
        if (nbyte <= 0 && errno != EWOULDBLOCK && errno != EAGAIN) {
          piafl_error("recv(waste): %m");
          fatalperror("recv(waste)");
          break;
        }
        clr_nonblock(isocket);
        blocked = 1;
      }
      else {
        piafl_error("recv(MSG_OOB): %m");
        fatalperror("recv(MSG_OOB)");
        break;
      }
    }

    switch( oob_byte ) {

    case '\3':                  /* keyboard interrupt from client */
      if (master)
         piafl_info("Hard interrupt");

      /*
       * When master server, propagate interrupt to slaves.
       * First five bits contain slave id, last three bits the oob_byte.
       */
      if (master && *active_slaves) {
         int i;
         char oobs;

         for (i = 0; i < *active_slaves; i++) {
            oobs = ((i+1)<<3) + oob_byte;
            piaf_sync(slave_isocket[i], &oobs);
         }
      }

      /*
       * flush input socket stream
       */
      while( 1 ) {
        int atmark;
        int nloop = 0;

        if( ioctl( isocket, SIOCATMARK, &atmark ) == -1 ) {
          piafl_error("ioctl(SIOCATMARK): %m");
          fatalperror("ioctl(SIOCATMARK)");
          break;
        }

        if( atmark != 0 ) {
          /*
           * send the OOB byte back that the client knows where to stop
           * flushing his input stream of obsolete messages
           */
          nbyte = send( osocket, &oob_byte, 1, MSG_OOB );
          if( nbyte <= 0 ) {
            piafl_error("send(MSG_OOB): %m");
            fatalperror("send(MSG_OOB)");
            break;
          }
          break;
        }

        set_nonblock(isocket);
        nbyte = recv( isocket, waste, (sizeof waste), 0 );
        if( nbyte <= 0 && errno != EWOULDBLOCK && errno != EAGAIN) {
          piafl_error("recv(waste): %m");
          fatalperror("recv(waste)");
          break;
        }
        clr_nonblock(isocket);
        if (oob_timeout(&nloop)) {
          piafl_error( "OOB timeout" );
          break;
        }
      }
      break;

    case '\4':                  /* soft interrupt */
      if (master)
         piafl_info("Soft interrupt");

      /*
       * When master server, propagate interrupt to slaves.
       * First five bits contain slave id, last three bits the oob_byte.
       */
      if (master && *active_slaves) {
         int i;
         char oobs;

         for (i = 0; i < *active_slaves; i++) {
            oobs = ((i+1)<<3) + oob_byte;
            piaf_sync(slave_isocket[i], &oobs);
         }
      }

      if( blocked ) {
        piafl_error("soft interrupt flushed stream");
        fatalperror("soft interrupt flushed stream");
        break;
      }
      /* set local soft interrupt flag and continue */
      ku_stop( 1 );
      return;
      break;

    default:
      piafl_error("unexpected OOB byte");
      fatalperror("unexpected OOB byte");
      break;
    }
    msg = NULL;
    break;

  case SIGHUP:
    /* for master logging is done in psmain */
    if (!master) {
       piafl_info("Soft kill received by slave (SIGHUP)");
       cleanup();
       exit(1);
    }
    /*
     * Send hard interrupt to slaves
     */
    if (master && *active_slaves) {
       int i;
       char oobs;

       for (i = 0; i < *active_slaves; i++) {
          oobs = ((i+1)<<3) + 3;
          piaf_sync(slave_isocket[i], &oobs);
       }
    }
    hangup = 1;
    msg = NULL;
    break;

  case SIGALRM:
    /* for master logging is done in psmain */
    if (!master) {
       piafl_info("Time-out received by slave (SIGALRM)");
       cleanup();
       exit(2);
    }
    /*
     * Send hard interrupt to slaves
     */
    if (master && *active_slaves) {
       int i;
       char oobs;

       for (i = 0; i < *active_slaves; i++) {
          oobs = ((i+1)<<3) + 3;
          piaf_sync(slave_isocket[i], &oobs);
       }
    }
    hangup = 2;
    msg = NULL;
    break;

  case SIGBUS:
    msg = "Bus error";
    break;

  case SIGFPE:
    msg = "Floating point exception";
    break;

  case SIGILL:
    msg = "Illegal instruction";
    break;

  case SIGSEGV:
    msg = "Segmentation violation";
    break;

  default:
    sprintf( umsg, "Unknown signal %d", sig );
    msg = umsg;
    break;
  }

  HBFREE(&in_use);

  if( msg != NULL ) {
    /* tell client about the exception on the server side */
    char line[80];
    INTEGER istat;
    if (master)
       sprintf(line, "BREAK* *** Break on Server %s *** %s", server_host, msg);
    else
       sprintf(line, "BREAK* *** Break on Slave %s *** %s", server_host, msg);
#ifdef __hpux
    U_STACK_TRACE();            /* somewhere in Fortran RTL */
#endif
    CALL_CZPUTA( line, &istat );
    PSPLOG( NULL, &istat );
  }

  siglongjmp( stack_env, sig );
}

/***********************************************************************
 *                                                                     *
 * Send a hard interrupt to all slaves whose bit are set in mask.      *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSINTR(INTEGER *smask)
{
   int     mask = *smask;

   if (master && *active_slaves) {
      int  i;
      char oobs;

      for (i = 0; i < *active_slaves; i++) {
         if ((mask & (1 << i))) {
            oobs = ((i+1)<<3) + 3;
            piaf_sync(slave_isocket[i], &oobs);
         }
      }
   }
}

/***********************************************************************
 *                                                                     *
 * Set the timeout after which Piaf will shut itself down.             *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSALARM(INTEGER *hour)
{
   unsigned int sec = *hour * 3600;

   alarm(sec);
}

/***********************************************************************
 *                                                                     *
 * Set pointers to some useful variables in common HCPIAF and write    *
 * begin of session info to syslog (done here because the cache size   *
 * is only known after PAWINT3 is called).                             *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSADDR(INTEGER *nslave, INTEGER *nslvpf, INTEGER *slinpf,
                  char *slhopf, int len_slhopf)
{
   active_slaves  = nslave;
   running_slaves = nslvpf;
   slave_isocket  = slinpf;
   slave_hosts    = slhopf;

   HBGMAX(&ulog.cache_size);
   if (master) piafl_begin();
}



/***********************************************************************
 *                                                                     *
 * Setup signal handler and loop on incoming messages.                 *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSMAIN(INTEGER *isock, INTEGER *osock)
{
  isocket = *isock;
  osocket = *osock;

  if (sigsetjmp( stack_env, 1 ) == 0) {
    struct sigaction act;
    int pid = getpid();

    /* install the signal handler */
    sigemptyset( &act.sa_mask );
    act.sa_flags = 0;
    act.sa_handler = signal_handler;
    sigaction( SIGHUP,  &act, NULL );
    sigaction( SIGBUS,  &act, NULL );
    sigaction( SIGFPE,  &act, NULL );
    sigaction( SIGILL,  &act, NULL );
    sigaction( SIGSEGV, &act, NULL );
    sigaction( SIGURG,  &act, NULL );
    sigaction( SIGALRM, &act, NULL );

    /* incoming OOB messages should generate SIGURG */
    if (ioctl( isocket, SIOCSPGRP, &pid ) == -1)
       piafl_error("ioctl(SIOCSPGRP): %m");

    /* send packets off immediately to minimize round-trip time */
    tcp_nodelay( isocket, 1 );
    /* use bigger TCP buffers */
    tcp_setbufs( isocket );
  }

  PSEXEC(&hangup);

}

/***********************************************************************
 *                                                                     *
 * Close connection                                                    *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSEXIT(INTEGER *isock)
{
   if (master) {
      piafl_summary();
      piafl_end(hangup);
   }

   cleanup();
   iclose(isock);
}

/***********************************************************************
 *                                                                     *
 * Get hostname.                                                       *
 * String host must be dimensioned to 16 characters.                   *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSHOST(char *host)
{
   struct utsname name;

   memset(host, ' ', 16);

   if ((uname(&name)) > -1) {
     strncpy(host, name.nodename, strlen(name.nodename));
     strcpy(server_host, name.nodename);
   }
}

/***********************************************************************
 *                                                                     *
 * Call system() with a given command line but use the ksh on Solaris  *
 *                                                                     *
 ***********************************************************************/
INTEGER PSSYSTEM(char *cmd,int len_cmd)
{
  char *s, *cc;
  INTEGER ret;
  size_t siz, i;

  siz = len_cmd + 30;
  cc = malloc(siz);
  s = malloc(siz);
  memset(cc,'\0',siz);
  memset(s,'\0',siz);
  strncpy(s,cmd,len_cmd);
  for (i = strlen(s); i > 0 && i == ' '; i--) ;
  s[i + 1] = '\0';
#if defined(CERNLIB_SUN)
  sprintf(cc,"/usr/bin/ksh '/usr/bin/%s '",s);
#else
  strcpy (cc,s);
#endif
  ret = system(cc);
  free(cc);
  free(s);
  return ret;
}

/***********************************************************************
 *                                                                     *
 * Call rm or sfrm depending on the fact if the object is a normal     *
 * file or a symlink. If sfrm is not found in PATH do not              *
 * delete any symlinks (they could not have been created anyway if     *
 * the DPM is not running).                                            *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSRM(char *tfile, int len_file)
{
  char   cmd[128], file[128];
  char  *ff, *fff;
  struct stat statbuf;
#if defined(CERNLIB_CORE)
  char *sfrm = ku_qexe("sfrm");
#else
#ifdef STAGEALLOC
  char *sfrm = "stageclr";
#else
  char *sfrm = NULL;
#endif
#endif

  /* store file name */
  strncpy(file, tfile, len_file);
  file[len_file] = '\0';
  ff = km_strip(file);

  /*
   * check validity of file name, may not contain wildcards and must be
   * a single file name
   */
  if (strchr(ff,'*') || strchr(ff,'?')) {
     printf(" Cannot remove files using wildcards\n");
     goto end;
  }
  if (strchr(ff,' ')) {
     printf(" Cannot remove multiple files in one go\n");
     goto end;
  }
  if (ff[strlen(ff)-1] == '/') {
     printf(" Not a file name %s\n", ff);
     goto end;
  }
  if (ff[0] == '-') {
     printf(" No options allowed\n");
     goto end;
  }

  /* get rid of any ~ */
  fff = ff;
  ff = fexpand(fff, NULL);
  free(fff);

  /* find out if file is a symlink */
  if (lstat(ff, &statbuf) == -1) {
     if (errno == ENOENT)
        printf("rm: %s non-existent\n", ff);
     goto end;
  }

  if (sfrm && S_ISLNK(statbuf.st_mode)) {
#ifdef STAGEALLOC
    char buf1[128];
    int rc;
    SetStgHost();
    if ( (rc = readlink (ff, buf1, 128)) != -1) {
      buf1[rc] = '\0';
      sprintf(cmd,"stageclr -h%s -P%s:%s",GetStgHost(),GetStgHost(),buf1);
    } else {
      printf(" Can't read link\n");
      goto end;
    }
#else
     /* Full pathname is not split in path & name anymore */
     sprintf(cmd, "sfrm -link%s %s", ".", ff);
#endif
  } else
     sprintf(cmd, "rm %s", ff);

  system(cmd);

end:
  free(ff);
}

/***********************************************************************
 *                                                                     *
 * Call simple cp when source not symlink otherwise we have to sfget   *
 * a new file and copy to that one.                                    *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSCP(char *tfile, int len_file)
{
  char   cmd[256], file[128];
  char  *fs, *fd, *s, *ff, *fff;
  struct stat statbuf;
#if defined(CERNLIB_CORE)
  char *sfget = ku_qexe("sfget");
#else
#ifdef STAGEALLOC
  char *sfget = "stagealloc";
#else
  char *sfget = NULL;
#endif
#endif


  /* store source and destination file names */
  strncpy(file, tfile, len_file);
  file[len_file] = '\0';
  ff = km_strip(file);

  if ((s = strchr(ff,' '))) {
     *s = '\0';
     fs = strdup(ff);
     *s = ' ';
     fff = strdup(s);
     fd = km_strip(fff);
     free(fff);
  } else {
     printf(" No destination specified\n");
     free(ff);
     return;
  }

  /*
   * check validity of file names, they may not contain wildcards and must be
   * a single file name
   */
  if (strchr(fs,'*') || strchr(fs,'?') ||
      strchr(fd,'*') || strchr(fd,'?')) {
     printf(" Cannot copy files using wildcards\n");
     goto end;
  }
  if (!strcmp(fs,fd)) {
     printf(" Source and destination are identical\n");
     goto end;
  }
  if (strchr(fd,' ')) {
     printf(" Cannot copy multiple files in one go\n");
     goto end;
  }
  if (fs[strlen(fs)-1] == '/') {
     printf(" Not a file name %s\n", fs);
     goto end;
  }
  if (fd[strlen(fd)-1] == '/') {
     printf(" Not a file name %s\n", fd);
     goto end;
  }
  if (fs[0] == '-' || fd[0] == '-') {
     printf(" No options allowed\n");
     goto end;
  }

  /* get rid of any ~ */
  fff = fs;
  fs = fexpand(fff, NULL);
  free(fff);
  fff = fd;
  fd = fexpand(fff, NULL);
  free(fff);

  /* find out if source file is a symlink */
  if (lstat(fs, &statbuf) == -1) {
     if (errno == ENOENT)
        printf("cp: %s non-existent\n", fs);
     goto end;
  }

  if (sfget && S_ISLNK(statbuf.st_mode)) {
     char  *path, fsbuf[128], fdbuf[128];
     off_t  fsize;
     int rc;

     /* get size of file symlink points at */
     if (stat(fs, &statbuf) == -1) {
        if (errno == ENOENT)
           printf("cp: %s non-existent\n", fs);
        goto end;
     }
     fsize = statbuf.st_size;


     fsize = (fsize / (1024*1024)) + 1;
#ifdef STAGEALLOC
     SetStgHost();
     /* If the destination link exists first remove link & data */
     rc = readlink(fd,fdbuf,128);
     if (rc != -1) {
       fdbuf[rc] = '\0';
       sprintf(cmd, "stageclr -h%s -P%s:%s",GetStgHost(),GetStgHost(),fdbuf);
       if (system(cmd) < 0) {
          perror( "Piaf server stageclr failed\n" );
	  goto end;
       } 
     } else {
	 if (errno != ENOENT) {
          perror( "Destination file is not valid" );
	  goto end;	   
	 }
     }
     sprintf(cmd, 
	     "stagealloc -h%s -p %s -s %d -u %s %s",
	     GetStgHost(),GetPool(),fsize,ulog.user,fd);
#else
     /* use sfget to create a destination file and link to the file */
     /* The full file name is not split in path and name anymore. Link is */
     /* created starting from the current directory      */
     sprintf(cmd, "sfget -r -q -s%dM -link%s %s", fsize, ".", fd);
#endif
     if (system(cmd) < 0) {
       printf ("Can't create destination file\n");
       goto end;
     }

#ifdef STAGEALLOC
     sprintf(cmd, "stageupdc -h%s %s",GetStgHost(),fd);
     if (system(cmd) < 0) {
       printf ("Can't exec stageupdc\n");
       goto end;
     }
#endif

     /* assume filenames are not longer than 128 !!!! */
     rc = readlink(fs, fsbuf, 128);
     if (rc != -1) {
       fsbuf[rc] = '\0';
     } else {
       printf (" Can't read source link\n");
       goto end;
     }
     rc = readlink(fd, fdbuf, 128);
     if (rc != -1) {
       fdbuf[rc] = '\0';
     } else {
       printf (" Can't read dest link\n");
       goto end;
     }

#if defined(CERNLIB_SOLARIS)
     /* Solaris doesn't allow -f option */
     sprintf(cmd, "cp %s %s", fsbuf, fdbuf);
#else
     sprintf(cmd, "cp -f %s %s", fsbuf, fdbuf); 
#endif

  } else
#if defined(CERNLIB_SOLARIS)
     /* Solaris doesn't allow -f option */
     sprintf(cmd, "cp  %s %s", fs, fd);
#else
     sprintf(cmd, "cp -f %s %s", fs, fd);
#endif

  system(cmd);

end:
  free(ff);
  free(fs);
  free(fd);
}

/***********************************************************************
 *                                                                     *
 * Call simple mv when source not symlink otherwise we have to cp to   *
 * a new file and delete the old one.                                  *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSMV(char *tfile, int len_file)
{
  char   cmd[256], file[128];
  char  *fs, *fd, *s, *ff, *fff;
  struct stat statbuf;

  /* store source and destination file names */
  strncpy(file, tfile, len_file);
  file[len_file] = '\0';
  ff = km_strip(file);

  if ((s = strchr(ff,' '))) {
     *s = '\0';
     fs = strdup(ff);
     *s = ' ';
     fff = strdup(s);
     fd = km_strip(fff);
     free(fff);
  } else {
     printf(" No destination specified\n");
     free(ff);
     return;
  }

  /*
   * check validity of file names, they may not contain wildcards and must be
   * a single file name
   */
  if (strchr(fs,'*') || strchr(fs,'?') ||
      strchr(fd,'*') || strchr(fd,'?')) {
     printf(" Cannot move files using wildcards\n");
     goto end;
  }
  if (!strcmp(fs,fd)) {
     printf(" Source and destination are identical\n");
     goto end;
  }
  if (strchr(fd,' ')) {
     printf(" Cannot move multiple files in one go\n");
     goto end;
  }
  if (fs[strlen(fs)-1] == '/') {
     printf(" Not a file name %s\n", fs);
     goto end;
  }
  if (fd[strlen(fd)-1] == '/') {
     printf(" Not a file name %s\n", fd);
     goto end;
  }
  if (fs[0] == '-' || fd[0] == '-') {
     printf(" No options allowed\n");
     goto end;
  }

  /* get rid of any ~ */
  fff = fs;
  fs = fexpand(fff, NULL);
  free(fff);
  fff = fd;
  fd = fexpand(fff, NULL);
  free(fff);

  /* find out if source file is a symlink */
  if (lstat(fs, &statbuf) == -1) {
     if (errno == ENOENT)
        printf("cp: %s non-existent\n", fs);
     goto end;
  }

  if (S_ISLNK(statbuf.st_mode)) {
     char fdbuf[128];
     int rc;

     /* if the destination file exists, first check it is a link! */
     rc = readlink(fd,fdbuf,128);
     if (rc == -1) {
       if (errno != ENOENT) {
          perror( "Destination file is not valid" );
	  goto end;	   
       }
     }
    
     /* if a file under DPM or SHIFT stager, first copy then remove file */
     PSCP(tfile, len_file);
     PSRM(fs, strlen(fs));

  } else {
     sprintf(cmd, "mv -f %s %s", fs, fd);
     system(cmd);
  }

end:
  free(ff);
  free(fs);
  free(fd);
}

/***********************************************************************
 *                                                                     *
 * Return one by one the files in the current Piaf directory.          *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSSCAN(char *ppath, int *lp, int *n, char *obname, char *obclas,
                  char *stext, char *ltext)
{
   char **f_desc, path[128];
   static char *expath;
   static int init = 0;

   if (!init) {
      km_file_type(".hbook", "hbook", "(Hbook File)", 0);
      init = 1;
   }

   memset(obname, ' ', 40);
   memset(obclas, ' ', 20);
   memset(stext,  ' ', 20);
   memset(ltext,  ' ', 80);

   if (*n == 0) {
      strncpy(path, ppath, *lp); path[*lp] = '\0';
      expath = fexpand(path, NULL);
   }

   if ((f_desc = km_scan_dir(expath, "*", *n, 0))) {
      if (f_desc[0])
         strncpy(obname, f_desc[0], strlen(f_desc[0]));
      if (f_desc[1]) {
         if (!strcmp(f_desc[1], "hbook"))
            strncpy(obclas, "piafhb", 6);
         else if (!strncmp(f_desc[1], "Dir", 3))
            strncpy(obclas, f_desc[1], strlen(f_desc[1]));
         else
            strncpy(obclas, "piafro", 6);
      }
      if (f_desc[2])
         strncpy(ltext,  f_desc[2], strlen(f_desc[2]));
      if (f_desc[3]) {
         if (!strcmp(f_desc[1], "hbook"))
            strncpy(stext, "(Piaf Hbook File)", 17);
         else if (!strncmp(f_desc[1], "Dir", 3))
            strncpy(stext,  f_desc[3], strlen(f_desc[3]));
         else
            strncpy(stext, "(Piaf R/O File)", 15);
      }
   } else
      free(expath);
}

/***********************************************************************
 *                                                                     *
 * Real and CPU time consumption                                       *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSTIME(INTEGER *what, INTEGER *nslave, INTEGER *id,
                  REAL *cptime, REAL *rltime)
{
  static clock_t rtime1;
  static clock_t ptime1;
  clock_t rtime;
  clock_t ptime;
  double elapsed;
  double cputime;
  struct tms tmsbuf;

  rtime = times( &tmsbuf );

  ptime = tmsbuf.tms_utime + tmsbuf.tms_stime
        + tmsbuf.tms_cutime + tmsbuf.tms_cstime;

  switch( *what ) {
  case 0:                       /* reset counters */
    rtime1 = rtime;
    ptime1 = ptime;
    break;
  case 1:                       /* print after each Ntuple command */
    elapsed = ( rtime - rtime1 ) / (double)CLK_TCK;
    cputime = ( ptime - ptime1 ) / (double)CLK_TCK;
    if (*id) {
       printf( " Piaf Slave %d: elapsed time %.3f, CPU time %.3f\n",
               *id, elapsed, cputime );
    } else {
       if (*nslave)
          printf( " Piaf Master:  elapsed time %.3f, CPU time %.3f\n",
                  elapsed, cputime );
       else
          printf( " Piaf elapsed time %.3f, CPU time %.3f\n",
                  elapsed, cputime );
    }
    break;
  case 2:
    elapsed = ( rtime - rtime1 ) / (double)CLK_TCK;
    cputime = ( ptime - ptime1 ) / (double)CLK_TCK;
    *rltime = (float) elapsed;
    *cptime = (float) cputime;
    break;
  }
}

/***********************************************************************
 *                                                                     *
 * Returns server user and cp time.                                    *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSCPU( REAL *suser, REAL *scpu )
{
  struct tms tmsbuf;

  clock_t rtime = times( &tmsbuf );
  clock_t utime = tmsbuf.tms_utime;
  clock_t stime = tmsbuf.tms_stime;

  double usrtime = utime / (double)CLK_TCK;
  double systime = stime / (double)CLK_TCK;

  *suser = (float) usrtime;
  *scpu  = (float) systime;
}

/***********************************************************************
 *                                                                     *
 * Returns the cluster load average.                                   *
 *                                                                     *
 ***********************************************************************/
void load_average(float *lav)
{
#ifdef HAVE_RSTAT

   char shosts[MAX_SLAVES][16];
   struct statstime stat;
   int  i;

   for (i = 0; i < 3; i++)
      lav[i] = 0.0;

   if (*active_slaves > 0) {

      for (i = 0; i < *active_slaves; i++) {
         int slen = 16;

         strncpy(shosts[i], slave_hosts+i*slen, slen);
         while (shosts[i][slen-1] == ' ')
            slen--;
         shosts[i][slen] = '\0';

         if (!(rstat(shosts[i], &stat))) {
            int j;

            for (j = 0; j < 3; j++)
               lav[j] += ((float)stat.avenrun[j])/250.0;
         }
      }
      for (i = 0; i < 3; i++)
         lav[i] /= *active_slaves;

   } else {
      if (!(rstat(server_host, &stat))) {
         for (i = 0; i < 3; i++)
            lav[i] = ((float)stat.avenrun[i])/250.0;
      }
   }
#endif
}

/***********************************************************************
 *                                                                     *
 * Print server status                                                 *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSSTAT( INTEGER *nslave, REAL *rzxio, REAL *svio,
                   REAL *suser, REAL *scpu )
{
  int  i;
  char shosts[MAX_SLAVES][16];
  struct tms tmsbuf;

  clock_t rtime = times( &tmsbuf );
  clock_t utime = tmsbuf.tms_utime;
  clock_t stime = tmsbuf.tms_stime;

  double usrtime = utime / (double)CLK_TCK + (double) *suser;
  double systime = stime / (double)CLK_TCK + (double) *scpu;

  int tick = ( rtime - rtime0 ) / CLK_TCK;
  int secs = ( tick % 60 );
  int mins = ( tick / 60 ) % 60;
  int hour = ( tick / 3600 );

  double rio = *rzxio * 4.e-6;
  double vio;
  float  words, lav[3];

  HBVM(&words);
  vio = (words + *svio) * 4.e-6;

  for (i = 0; i < *active_slaves; i++) {
     int slen = 16;
     strncpy(shosts[i], slave_hosts+i*slen, slen);
     while (shosts[i][slen-1] == ' ')
        slen--;
     shosts[i][slen] = '\0';
  }

  /* get the cluster load average */
  load_average(lav);

  /* cat_motd( server_version ); */

  printf( "Piaf server status:\n" );
  printf( "===================\n" );
  printf( "User name:          %s@%s\n", real_user_name, client_host );
  printf( "Working directory:  %s\n", workdir );
  printf( "Server host / PID:  %s / %d\n", server_host, getpid() );
  printf( "Server tag      :   %d\n", GetPiafVersion ());
  printf( "VMEM size       :   %sM\n",VmemSize);
  printf( "Protocol version:   %s / %d\n", server_version, server_protocol );
  if (*nslave == 0)
     printf( "Processing mode:    sequential\n" );
  else {
     printf( "Processing mode:    parallel (%d slaves)\n", *nslave );
     printf( "Slave hosts:        ");
     for (i = 0; i < *active_slaves; i++)
        printf("%s ", shosts[i]);
     printf("\n");
  }
  printf( "Connection time:    %.2d:%.2d:%.2d\n", hour, mins, secs );
  printf( "User / system time: %.3f / %.3f sec\n", usrtime, systime );
  printf( "Real / virtual I/O: %.3f / %.3f MB\n", rio, vio );
#ifdef HAVE_RSTAT
  printf( "Load average:       %.2f, %.2f, %.2f\n", lav[0], lav[1], lav[2]);
#endif
  stginfo();
}

/***********************************************************************
 *                                                                     *
 * Remove the log file.                                                *
 *                                                                     *
 ***********************************************************************/
static void cleanup(void)
{
#if 0
   int errsav = errno;
   remove( logfile );
   errno = errsav;
#endif
}

/***********************************************************************
 *                                                                     *
 * Send an error message back to the client.                           *
 *                                                                     *
 ***********************************************************************/
static void fatal(char *msg)
{
   char          buf[BUFSIZ];
   int           ntot, istat;

   sprintf(buf, "piafserv: %s.", msg);
   CALL_CZPUTA(buf, &istat);
   exit(1);
}

/***********************************************************************
 *                                                                     *
 * Fatal error, as in fatal, but include the errno value in the msg.   *
 *                                                                     *
 ***********************************************************************/
static void fatalperror(char *msg)
{
   char          buf[BUFSIZ];
#if !defined(CERNLIB_QMGLIBC)
   extern int    sys_nerr;
   extern char  *sys_errlist[];
#endif

   if ((unsigned) errno < sys_nerr)
      sprintf(buf, "%s: %s", msg, sys_errlist[errno]);
   else
      sprintf(buf, "%s: Error %d", msg, errno);

   fatal(buf);
}

/***********************************************************************
 *                                                                     *
 * Client wants to receive a text file.                                *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSGETA( INTEGER *iskin, INTEGER *loglevel )
{
  int sockfd = *iskin;
  char fname[256];

  if( recv_buf( sockfd, fname, (sizeof fname) ) > 0 ) {
    char *fexname = fexpand(fname, NULL);
    int   fildes  = open( fexname, O_RDONLY );
    free(fexname);

    if( fildes == -1 ) {
      send_str( sockfd, fname );
    }
    else {
      int nread;
      int nbytes = 0;
      int nblock = 0;
      int status = 0;
      struct tms tmsbuf;
      clock_t rtime = times( &tmsbuf );
      double elapsed, kbsec;
      unsigned char recl0, recl1, nhand;
      int reclen, ticks;
      char *buf;

      /* acknowledgement that open went okay */
      send_eof( sockfd );

      /* receive the maximum block size the client can handle */
      recv( sockfd, (char *)&recl0, 1, 0 );
      recv( sockfd, (char *)&recl1, 1, 0 );
      reclen = (recl1 << 8) + recl0;
      buf = malloc( reclen );

      /* receive hand shake frequency */
      recv( sockfd, (char *)&nhand, 1, 0 );

      tcp_nodelay( sockfd, 0 );
      while( (nread = read( fildes, buf, reclen )) > 0 ) {

        send_buf( sockfd, buf, nread );
        nbytes += nread;

        if( ++nblock == nhand ) {
          char nokay;
          recv( sockfd, &nokay, 1, 0 );
          if( nokay != 0 ) {
            printf( " *** Transfer aborted by client\n" );
            break;
          }
          nblock = 0;
        }
      }
      if( nread == -1 ) {
        perror( "Piaf server read failed" );
      }
      tcp_nodelay( sockfd, 1 );

      free( buf );
      send_eof( sockfd );
      close( fildes );

      ticks = times( &tmsbuf ) - rtime;
      elapsed = (ticks == 0 ? 1 : ticks) / (double)CLK_TCK;
      kbsec = nbytes * 0.001 / elapsed;
      piafl_geta(nbytes, (float)elapsed, (float)kbsec);

      if( *loglevel >= 2 || nbytes > 100000 ) {
        printf( "Transfer completed: %d bytes in %.3f seconds = %.1f kB/sec\n",
               nbytes, elapsed, kbsec );
      }
    }
  }
}

#if defined(CERNLIB_RFIO)
extern int      rfio_open(char *, int, int);    /* RFIO's open()    */
extern int      rfio_close(int);                /* RFIO's close()   */
extern int      rfio_write(int, char *, int);   /* RFIO's write()   */
#define open    rfio_open
#define close   rfio_close
#define write   rfio_write
#endif

/*
 * A special thing for the Meiko CS-2:  Use special ioctl's
 * to speed up writing to a parallel file system.  In the best
 * case we can get close to 10 MB/s with this stuff...
 */
#ifdef MEIKO_CS2

# include <sys/statvfs.h>
# include <nfs/nfsio.h>
static void
setpfs(int fd,long bufsiz)
{
  struct statvfs fsbuf;

  /* turning off client cache helps for buffers > 16K 
  if (bufsiz > 16 * 1024 &&
      fstatvfs(fd,&fsbuf) >= 0 &&
      !strcmp(fsbuf.f_basetype,"pfs"))
    ioctl(fd,NFSIOCACHE,CLIENTNOCACHE); */
    ioctl(fd,NFSIOCACHE,CLIENTCACHE);
}

# define USER_FMODE 0640 /* to allow administrators easier access... */

#else
# define USER_FMODE 0640
#endif

/***********************************************************************
 *                                                                     *
 * Client wants to push a text file.                                   *
 *                                                                     *
 * When the DPM is installed we want to place all Hbook files under    *
 * DPM control (using the command "sfget"). To find out if a file is   *
 * an Hbook file we need to look for the hbook_key (integer encoded    *
 * value of the string HBOOK-ID) in the first few hundred bytes of the *
 * second record of a file. This means that we have to buffer up to    *
 * KEY_LIMIT bytes before we can decide how to open the new file (i.e. *
 * locally or under DPM control). It would have been so much easier    *
 * if all RZ files would have contained a unique ID in the first few   *
 * hundred bytes.                                                      *
 * ################################################################### *
 * On the CS2 the DPM is replaced either by Timo's PFGET daemon or by  *
 * SHIFT's STAGE utilities (stagealloc,stageclr).                      *
 * Default is now SHIFT's stage utilities                              *
 * ################################################################### *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSPUTA( INTEGER *iskin, INTEGER *loglevel, INTEGER *fs )
{
  int  sockfd = *iskin;
  int  fsize  = *fs;
  char fname[256];
  struct stat sbuf;

  if (recv_buf( sockfd, fname, (sizeof fname) ) > 0) {
    char *fexname, *fsname;
    int   fildes  = 0, askfordir = 0;
    static char nodirs[] = "\
(Target file cannot be a directory - give full path instead)";

    char *sfget = NULL;

#if defined(CERNLIB_CORE)
    sfget = ku_qexe("sfget");
#endif

#ifdef PFGET
    sfget = "pfget";
#endif

#ifdef STAGEALLOC
    /* The default stager */
    sfget = "stagealloc";
#endif

    if ((fsname = strchr(fname,' ')) != NULL) {
      *fsname = '\0';
      for (fsname++; *fsname == ' '; fsname++) ;
      if (*fsname == '\0')
        fsname = NULL;
    }
    fexname = fexpand(fname, NULL);
    /* if the target has a slash as last char, user means a dir */
    if (fexname[strlen(fexname) - 1] == '/') {
      /* old protocol, return error */
      if (!fsname) {
        send_str(sockfd,nodirs);
        free(fexname);
        return;
      }
      fexname[strlen(fexname) - 1] = '\0';
      askfordir = 1;
    }
    if (stat(fexname,&sbuf) == 0 && !S_ISREG(sbuf.st_mode)) {
      if (S_ISDIR(sbuf.st_mode)) {
        /* old protocol, return error */
        if (!fsname) {
          send_str(sockfd,nodirs);
          free(fexname);
          return;
        }
        /* glue the directory path and file name together */
        else {
          char *s = malloc(strlen(fexname) + strlen(fsname) + 2);
          strcpy(s,fexname);
          strcat(s,"/");
          strcat(s,fsname);
          free(fexname);
          fexname = s;
        }
      }
      /* some other special file - no good */
      else {
        char tbuf[80];
        sprintf(tbuf,"(Target must be a regular file%s)",
          fsname ? " or a directory" : "");
        send_str(sockfd,tbuf);
        free(fexname);
        return;
      }
    }
    else if (askfordir) {
      send_str(sockfd,"(Target is not a directory)");
      free(fexname);
      return;
    }

    { /* if a directory is specified, check it does exist */
      struct stat mystat;
      char *s, *apath;
      apath = strdup(fexname);
      if ((s = strrchr(apath,'/'))) {
	*s = '\0';
	if (stat(apath,&mystat) == -1) {
	  send_str(sockfd,"(Directory does not exist)");
	  free(fexname);
	  free(apath);
	  return;
	}
      }
      free(apath);
    }

    /* No DPM running */
    if (!sfget) {
       fildes = open( fexname, O_WRONLY|O_CREAT|O_TRUNC, 0640 );
       free(fexname);
    }

    if ( fildes == -1 ) {
      send_str( sockfd, fname );
    }
    else {
      int nbytes = 0;
      int nblock = 0;
      int nbuf   = 0;
      int opened = 0;
#ifdef MEIKO_CS2
      int ntot = 0;
#endif
      struct tms tmsbuf;
      clock_t rtime = times( &tmsbuf );
      double elapsed, kbsec;
      unsigned char recl0, recl1, nhand, nokay;
      int   reclen, nrecv, ticks;
      int   lb[MAX_BUF];
      char *buf[MAX_BUF];

      /*
       * Acknowledge successful creation of file (even if file has not yet
       * been created)
       */
      send_eof( sockfd );

      /* receive the maximum block size the client can handle */
      recv( sockfd, (char *)&recl0, 1, 0 );
      recv( sockfd, (char *)&recl1, 1, 0 );
      reclen = (recl1 << 8) + recl0;
#ifdef MEIKO_CS2
      buf[0] = malloc( (reclen + 1) * MAX_BUF );
      if (fildes > 0)
        setpfs(fildes,(long)((reclen + 1) * MAX_BUF));
#else
      buf[nbuf] = malloc( reclen + 1 );
#endif

      /* receive hand shake frequency */
      recv( sockfd, (char *)&nhand, 1, 0 );

      nokay = 0;
      while ((nrecv = lb[nbuf] = recv_buf(sockfd, buf[nbuf], reclen+1)) > 0) {
#ifdef MEIKO_CS2
        ntot += nrecv - 1;
#endif
        if (nokay == 0) {

           /* No DPM running, write immediately */
           if (!sfget && write(fildes, buf[nbuf], lb[nbuf]-1) != lb[nbuf]-1) {
              perror( "Piaf server write failed" );
              nokay = 1;

           } else if (sfget) {
              /*
               * If DPM running and file not yet opened and not yet received
               * KEY_LIMIT bytes, search for hbook_key.
               */
              if (!opened && nbytes < KEY_LIMIT) {
                 int i;
                 /* static int hbook_key[2] = { 3993736, 1087883 }; */
                 static unsigned char hbook_key[8] =
                                          { 0, 60, 240, 136, 0, 16, 153, 139 };

                 for (i = 0; i < lb[nbuf] - 1; i++) {
                    if (!memcmp(&buf[nbuf][i], hbook_key, 8)) {
                       int   j;
                       char *s, *path;
                       char  cmd[512];

                       /* get file name from DPM and open file */
                       if ((s = strrchr(fexname,'/'))) {
                          *s = '\0';
                          path = strdup(fexname);
                          *s = '/';
                          s++;
                       } else {
                          s = fexname;
                          path = strdup(".");
                       }

#ifdef STAGEALLOC
                       { /* stagealloc default stager */
                          int rc;
			  char buf1[128];
			  SetStgHost();  
			  /* If the link exists first remove link & data */
                          rc = readlink(fexname,buf1,128);
			  if (rc != -1) {
			    buf1[rc] = '\0';
			    sprintf(cmd, "stageclr -h%s -P%s:%s",
                               GetStgHost(),GetStgHost(),buf1);
			    if (system(cmd) < 0) {
			      perror( "Piaf server stageclr failed\n" );
			      nokay = 1;
			    }
			  }
		       }
#endif
                       if (fsize > 0) {
                          fsize = (fsize / (1024*1024)) + 1;
#ifdef PFGET
                          sprintf(cmd,
                "ln -sf `%s/bin/pfget -r -s%dM %s 2>> %s/log/pfget.err` %s/%s",
                                  conf_dir, fsize, s, conf_dir, path, s);
#else
#if defined (CERNLIB_CORE)
                          /* fexname is not split in path & name anymore */
                          sprintf(cmd, "sfget -r -s%dM -link%s %s",
                                  fsize, ".", fexname);
#else
#ifdef STAGEALLOC
                          /* stagealloc default stager */
			  sprintf(cmd, 
			     "stagealloc -h%s -p %s -s %d -u %s %s",
			     GetStgHost(),GetPool(),fsize,
                             ulog.user,fexname);
#endif
#endif
#endif
                       } else
#ifdef PFGET
                          sprintf(cmd,
                "ln -sf `%s/bin/pfget -r -s50M %s 2>> %s/log/pfget.err` %s/%s",
                                  conf_dir, s, conf_dir, path, s);
#else
#if defined (CERNLIB_CORE)
                          /* fexname is not split in path & name anymore */
                          sprintf(cmd, "sfget -r -s50M -link%s %s", ".",
                                  fexname );
#else
#ifdef STAGEALLOC
                          /* stagealloc default stager */
			  sprintf(cmd, 
			     "stagealloc -h%s -p %s -s 50 -u %s %s",
			     GetStgHost(),GetPool(),ulog.user,fexname);
#endif
#endif
#endif

                       if (system(cmd) < 0 ||
                         (fildes = open(fexname, O_WRONLY, USER_FMODE)) < 0) {
                          perror( "Piaf server open failed" );
                          nokay = 1;
                       }

#ifdef STAGEALLOC
		       sprintf(cmd,"stageupdc -h%s %s",GetStgHost(),fexname);
		       if (system(cmd) < 0) {
                          perror( "Piaf server stageupdc failed" );
                          nokay = 1;
                       }
#endif
                      chmod(fexname, USER_FMODE);
                       free(fexname);
                       free(path);

#ifdef MEIKO_CS2
                       setpfs(fildes,(long)((reclen + 1) * MAX_BUF));
                       nbuf++;
                       buf[nbuf] = buf[0] + ntot;
#else
                       /* flush buffers to file */
                       for (j = 0; j <= nbuf; j++) {
                          if (!nokay &&
                              write(fildes, buf[j], lb[j]-1) != lb[j]-1) {
                             perror( "Piaf server write failed" );
                             nokay = 1;
                          }
                       }

                       /* free buffers, keep only buf[0] */
                       while (nbuf > 0) {
                          free(buf[nbuf]);
                          nbuf--;
                       }
#endif

                       opened = 1;
                       break;
                    }
                 }

                 /* hbook_key not (yet) found, create new buffer */
                 if (!opened) {
                    nbuf++;
#ifdef MEIKO_CS2
                    buf[nbuf] = buf[0] + ntot;
#else
                    buf[nbuf] = malloc(reclen + 1);
#endif
                 }
              } else if (!opened) {
                 /*
                  * If no hbook_key has been found within the first KEY_LIMIT
                  * bytes, this is no hbook file. Open the file locally and
                  * flush the buffers.
                  */
                 int i;

                 fildes = open(fexname, O_WRONLY|O_CREAT|O_TRUNC, USER_FMODE);
                 if (fildes == -1) {
                    perror("Piaf server open failed");
                    nokay = 1;
                 }
                 free(fexname);

#ifdef MEIKO_CS2
                 setpfs(fildes,(long)((reclen + 1) * MAX_BUF));
                 if (write(fildes, buf[0], ntot) != ntot) {
                    perror( "Piaf server write failed" );
                    nokay = 1;
                 }
                 ntot = nbuf = 0;
#else
                 /* flush buffers to file */
                 for (i = 0; i <= nbuf; i++) {
                    if (!nokay &&
                        write(fildes, buf[i], lb[i]-1) != lb[i]-1) {
                       perror( "Piaf server write failed" );
                       nokay = 1;
                    }
                 }

                 /* free buffers, keep only buf[0] */
                 while (nbuf > 0) {
                    free(buf[nbuf]);
                    nbuf--;
                 }
#endif

                 opened = 1;

              /*
               * File has been opened, write buffer.
               */
#ifdef MEIKO_CS2
              } else if (ntot >= KEY_LIMIT) {
                 if (write(fildes, buf[0], ntot) != ntot) {
                    perror( "Piaf server write failed" );
                    nokay = 1;
                 }
                 ntot = nbuf = 0;
              }
              else {
                 nbuf++;
                 buf[nbuf] = buf[0] + ntot;
              }
#else
              } else if (write(fildes, buf[nbuf], lb[nbuf]-1) != lb[nbuf]-1) {
                 perror( "Piaf server write failed" );
                 nokay = 1;
              }
#endif
           }
        }
        nbytes += nrecv - 1;

        if (++nblock == nhand) {
          send( sockfd, (char *)&nokay, 1, 0 );
          if (nokay != 0) {
            printf( " *** Transfer aborted by server\n" );
            continue;           /* client still sends EoF record */
          }
          nblock = 0;
        }
      }

      /*
       * If DPM running and file shorter than KEY_LIMIT, open file and
       * write locally (this is not an Hbook file).
       */
      if (sfget && !opened) {
          int i;

          fildes = open(fexname, O_WRONLY|O_CREAT|O_TRUNC, USER_FMODE);
          if (fildes == -1) {
             perror("Piaf server open failed");
             nokay = 1;
          }
          free(fexname);

#ifdef MEIKO_CS2
          setpfs(fildes,(long)((reclen + 1) * MAX_BUF));
          if (write(fildes, buf[0], ntot) != ntot) {
             perror( "Piaf server write failed" );
             nokay = 1;
          }
#else
          /* flush buffers to file */
          for (i = 0; i < nbuf; i++) {
             if (!nokay && write(fildes, buf[i], lb[i]-1) != lb[i]-1) {
                perror( "Piaf server write failed" );
                nokay = 1;
             }
          }

          /* free tmp buffers, keep only buf[0] */
          while (nbuf > 0) {
              free(buf[nbuf]);
              nbuf--;
          }
#endif
      }
#ifdef MEIKO_CS2
      /* write the last short part */
      else if (ntot > 0) {
        if (write(fildes, buf[0], ntot) != ntot) {
          perror( "Piaf server write failed" );
          nokay = 1;
        }
      }
#endif

      free( buf[0] );
      close( fildes );

      ticks = times( &tmsbuf ) - rtime;
      elapsed = (ticks == 0 ? 1 : ticks) / (double)CLK_TCK;
      kbsec = nbytes * 0.001 / elapsed;
      piafl_puta(nbytes, (float)elapsed, (float)kbsec);

      if ( *loglevel >= 2 || nbytes > 100000 ) {
        printf( "Transfer completed: %d bytes in %.3f seconds = %.1f kB/sec\n",
               nbytes, elapsed, kbsec );
      }
    }
  }
}

#if defined(CERNLIB_RFIO)
#undef open
#undef close
#undef write
#endif

/***********************************************************************
 *                                                                     *
 * Multicast equivalent of CZPUTA.                                     *
 *                                                                     *
 * The message MSG is multicast to the N slave servers specified in    *
 * the ISKIN list of socket descriptors.                               *
 *                                                                     *
 * The array bad contains *istat indices into the array iskin of       *
 * bad (not responding within time limit) slaves. Note that the        *
 * indices are +1 since the array is supposed to be used in a Fortran  *
 * routine.                                                            *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSMPUT(char *msg, INTEGER *n, INTEGER *iskin, INTEGER *bad,
                  INTEGER *istat, int len_msg)
{
   int    *sockfd = iskin;
   int    *bad_sockfd = bad;
   int     nslave = *n;
   char    line[80], ackn[4];
   int     len, i, got_one, timeout, cont;
   int     nfd = 0;
   fd_set  read_fd, bad_fd, ready;
   struct timeval  tout;

   *istat = 0;
   if (!nslave) return;

   len = MIN(len_msg, 80);
   strncpy(line, msg, len);
   memset(line+len, ' ', sizeof(line)-len);

   FD_ZERO(&read_fd);
   FD_ZERO(&bad_fd);

   /*
    *  Multicast message
    *  In bad_fd keep track of bad slaves
    */
   for (i = 0; i < nslave; i++) {
      FD_SET(sockfd[i], &read_fd);
      nfd = MAX(sockfd[i], nfd);

      if (send_len(sockfd[i], line, sizeof(line)) == -1) {
         FD_SET(sockfd[i], &bad_fd);
         FD_CLR(sockfd[i], &read_fd);
      }
   }

   /*
    *  Check if the send was not a total failure
    */
   nfd++;
   cont = 0;
   for (i = 0; i < nfd; i++)
      if (FD_ISSET(i, &read_fd)) {
         cont = 1;
         break;
      }

   if (!cont) goto bad;


   /*
    *  Wait for acknowledgement from all slaves
    */
   tout.tv_sec  = TIMEOUT;
   tout.tv_usec = 0;

   got_one = 0;
   timeout = 0;

   for ( ; ; ) {
      int nr;

      ready = read_fd;
      if ((nr = select(nfd, FDT &ready, FDT 0, FDT 0, &tout)) < 0) {
         piafl_info("*** PSMPUT: select failed ***");
         for (i = 0; i < nfd; i++)
            if (FD_ISSET(i, &read_fd)) FD_SET(i, &bad_fd);
         break;
      }

      /*
       *  In case of timeout, and no slave has responded yet, wait again
       *  (repeat this MAX_TIMEOUT times).
       *  In case some slave(s) already responded consider all other slaves
       *  dead and put them in the bad list.
       */
      if (nr == 0) {
         char lmesg[128];
         sprintf (lmesg,
            "*** PSMPUT: time-out *** MSG: %s *** LENMSG %d",line,len_msg);
         piafl_info(lmesg);
         timeout++;
         /* if (got_one || timeout >= MAX_TIMEOUT) { */
         if (timeout >= MAX_TIMEOUT) {
            sprintf (lmesg,
               "*** PSMPUT:  MAX_TIMEOUT reached *** MSG: %s *** LENMSG %d",
               line,len_msg);
            piafl_info(lmesg);
            for (i = 0; i < nfd; i++)
               if (FD_ISSET(i, &read_fd)) FD_SET(i, &bad_fd);
            break;
         } else
            continue;
      }

      /*
       *  Receive the acknowledgement
       */
      for (i = 0; i < nfd; i++) {
         if (FD_ISSET(i, &ready)) {
            if (recv_len(i, ackn, sizeof(ackn)) == -1)
               FD_SET(i, &bad_fd);
            FD_CLR(i, &read_fd);
            got_one = 1;
         }
      }

      /*
       *  Did we get all acknowledgements? If not continue waiting for them
       */
      cont = 0;
      for (i = 0; i < nfd; i++)
         if (FD_ISSET(i, &read_fd)) {
            cont = 1;
            break;
         }

      if (!cont) break;

   }

bad:
   for (i = 0; i < nfd; i++)
      if (FD_ISSET(i, &bad_fd)) {
         int j;
         for (j = 0; j < nslave; j++)
            if (sockfd[j] == i) bad_sockfd[*istat] = j+1;
         (*istat)++;
      }
}

/***********************************************************************
 *                                                                     *
 * Multicast equivalent of CZPUTC.                                     *
 *                                                                     *
 * The NT characters from buffer CZBUFC are multicast to the N         *
 * slave servers specified in the ISKIN list of socket descriptors.    *
 *                                                                     *
 * The array bad contains *istat indices into the array iskin of       *
 * bad slaves.  Note that the indices are +1 since the array is        *
 * supposed to be used in a Fortran routine.                           *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSMBUF(INTEGER *nt, INTEGER *n, INTEGER *iskin, INTEGER *bad,
                  INTEGER *istat)
{
   int    *sockfd = iskin;
   int    *bad_sockfd = bad;
   int     ntot = *nt;
   int     nslave = *n;
   int     i;

   *istat = 0;
   if (!nslave) return;

   /*
    *  Multicast message
    *  In bad_sockfd keep track of bad slaves
    */
   for (i = 0; i < nslave; i++) {
      if (send_len(sockfd[i], CZBUFC, ntot) == -1) {
         bad_sockfd[*istat] = i+1;
         (*istat)++;
      }
   }
}

/***********************************************************************
 *                                                                     *
 * Wait for input on one of the N slave servers specified in the       *
 * ISKIN list of socket descriptors.                                   *
 * The select() is only done on the slave servers whose bit is set in  *
 * the smask.                                                          *
 *                                                                     *
 * The array bad contains *istat indices into the array iskin of       *
 * bad (not responding within time limit) slaves. Note that the        *
 * indices are +1 since the array is supposed to be used in a Fortran  *
 * routine.                                                            *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSMSEL(INTEGER *n, INTEGER *smask, INTEGER *iskin,
                  INTEGER *islave, INTEGER *bad, INTEGER *istat)
{
   int    *sockfd = iskin;
   int    *bad_sockfd = bad;
   int     nslave = *n;
   int     mask = *smask;
   int     i;
   int     nfd = 0;
   fd_set  read_fd, bad_fd, ready;

   *istat  = 0;
   *islave = 0;

   if (!nslave) return;

   /*
    *  Initialize the file descriptor sets
    */
   FD_ZERO(&read_fd);
   FD_ZERO(&bad_fd);

   for (i = 0; i < nslave; i++) {
      if ((mask & (1 << i))) {
         FD_SET(sockfd[i], &read_fd);
         nfd = MAX(sockfd[i], nfd);
      }
   }
   nfd++;

   ready = read_fd;
   if (select(nfd, FDT &ready, FDT 0, FDT 0, (struct timeval *) 0) < 0) {
      for (i = 0; i < nfd; i++)
         if (FD_ISSET(i, &read_fd)) {
            int j;
            for (j = 0; j < nslave; j++)
               if (sockfd[j] == i) bad_sockfd[*istat] = j+1;
            (*istat)++;
         }
   } else {
      for (i = 0; i < nfd; i++) {
         if (FD_ISSET(i, &ready)) {
            int j;
            for (j = 0; j < nslave; j++) {
               if (sockfd[j] == i) {
                  *islave = j+1;
                  return;
               }
            }
         }
      }
   }
}

void
cunpackgcut (int ACut, char *CBuf, int Np,char *Points,int *errp)
{
   int i;
   *errp = R_NOERR;
   if (Np == 2) {
      float xlow,xhigh;
      sscanf (Points, "%12.6g12.6g",&xlow,&xhigh);
      gcut_add_1d (ACut,CBuf,xlow,xhigh);
   } else {
      float *xv,*yv;
      char *exprx,*expry;
      char tmp[16],*ptr;
      exprx = malloc (strlen(CBuf));
      if ( exprx == 0) {
	 sf_report("cunpackgcut: Allocation error");
	 *errp = R_ALLOC_ERROR;
	 return;
      }
      expry = malloc (strlen(CBuf));
      if ( expry == 0) {
	 sf_report("cunpackgcut: Allocation error");
	 *errp = R_ALLOC_ERROR;
	 return;
      }
      xv = calloc (sizeof(float),Np);
      if ( xv == 0) {
	 sf_report("cunpackgcut: Allocation error");
	 *errp = R_ALLOC_ERROR;
	 return;
      }
      yv = calloc (sizeof(float),Np);
      if ( yv == 0) {
	 sf_report("cunpackgcut: Allocation error");
	 *errp = R_ALLOC_ERROR;
	 return;
      }
      strcpy (exprx,strtok (CBuf,"%"));
      strcpy (expry,strtok (NULL,"%"));
      ptr = Points;
      for (i = 0; i < Np ; i++) {
	 memcpy (tmp,ptr,12);
	 tmp[12] = '\0';
	 ptr += 12;
	 xv [i] = atof (tmp);
	 memcpy (tmp,ptr,12);
	 tmp[12] = '\0';
	 ptr += 12;
	 yv [i] = atof (tmp);
      }
      gcut_add_2d (ACut,exprx,expry,Np,xv,yv);
      free (xv);
      free (yv);
      free(exprx);
      free(expry);
   }
}

FCALLSCSUB5(cunpackgcut,UNPACKGCUT,unpackgcut,INT,STRING,INT,STRING,PINT)


void DecodeMsg (char *Message, int *retval)
{
   char s1[32],s2[32];
   int pv;
   if (strstr (Message,PQRY_KEYW) != NULL) {
      *retval = 1;
      /* There is a meaningful info */
      if (strstr (Message,VERS_KEYW) != NULL) {
	 /* Querying the PIAF version */
	 sprintf (Message,"%s %s %d", PQRY_KEYW,VERS_KEYW,GetPiafVersion ());
      }
   } else {
      *retval = 0;
   }
}

FCALLSCSUB2(DecodeMsg,DECODEMESS,decodemess,PSTRING,PINT)


void GetVmemSize (char *vsize, int *retval)
{
  if (strcmp (VmemSize, DEFAULT_VMEM_SIZE) != 0) {
      *retval = 1;
      strcpy (vsize, VmemSize);
   } else {
      *retval = 0;
   }
}

FCALLSCSUB2(GetVmemSize,VMSIZE,vmsize,PSTRING,PINT)

#ifdef STAGEALLOC
/* Returns the Stager's Pool Name according to the group the */
/* user belongs to */

char *GetPool () {
  struct group *gp;
  gid_t op = getgid();
  gp = getgrgid (op);
  if ( strcmp (gp->gr_name,NA49GRP) == 0) {
    return NA49POOL;
  }
  else {
    return STDPOOL;
  }
}


/* Read STAGE HOST name from shift.conf file */
char *SetStgHost () {
  FILE *shiftconf;

  if( (shiftconf = fopen( STAGECONFIG, "r" )) != NULL ) {
    /* read shift configuration file */
    char line[256];
    int found = 0;

    while( (fgets( line,256,shiftconf ) != NULL)&&( !found ) ) {
      char word[3][64];
      sscanf( line, " %s %s %s",word[0], word[1], word[2]);
      if (!strncmp(word[0],STGKEYW,STGKEYWL)) {
	if (!strncmp(word[1],HOSTKEYW,HOSTKEYWL)) {
	  strncpy (stage_host,word[2],sizeof(stage_host)-1);
	  found = 1;
	}
      }
    }
    /* stupid trick: the STAGE HOST should always be there! */
    if (!found)
      strcpy (stage_host,"HOSTNOTFOUND");

    fclose (shiftconf);
  }
  return stage_host;
}

char *GetStgHost () {
  return stage_host;
}
#endif

/* A function to check user quota; if the proper script is found */
/* it is executed */

void CheckQuota () {
  char fname[128];
  FILE *quoter;
  sprintf (fname,"%s/%s", conf_dir, QUOTER);
  if( (quoter = fopen(fname , "r" )) != NULL ) {
    fclose (quoter);
    if (system(fname) < 0 )
      perror ("Cannot check user quota\n");
  }
}

FCALLSCSUB0(CheckQuota,CQUOTA,cquota)
