/*
 * $Id: tcpiaf.c,v 1.13 1999/09/15 12:59:57 mclareni Exp $
 *
 * $Log: tcpiaf.c,v $
 * Revision 1.13  1999/09/15 12:59:57  mclareni
 * Remove strdup redefinition
 *
 * Revision 1.12  1999/06/28 15:28:24  couet
 * - use cfortran.h from $CVSCOSRC
 *
 * Revision 1.11  1997/10/23 12:41:58  mclareni
 * cpp defines for Multinet and DECC on VMS
 *
 * Revision 1.10  1996/09/17 08:31:58  dinofm
 * Fortran wrappers LGETUS and MAXCUTS were incorrectly declared as functions
 * with one parameter (FCALLSCFUN1) although no parameter was passed; this
 * caused a deadlock in the PFPAW function on the CS-2 when the library was
 * compiled with cc V4.0.
 * Now both of them are declared as FCALLSCFUN0 wrappers.
 *
 * Revision 1.9  1996/05/06 13:33:23  dinofm
 * Code modified to take care of empty histograms detection on slave(s).
 * ----------------------------------------------------------------------
 * CVS:
 * ----------------------------------------------------------------------
 *
 * Revision 1.8  1996/04/29 09:47:50  dinofm
 * Bug fixed in function UnionSetLen (missing return statement)
 *
 * Revision 1.7  1996/04/18 12:10:24  dinofm
 * Code to pack/unpack labels in strings has been moved in from smap.c
 *
 * Revision 1.6  1996/04/18 08:51:38  dinofm
 * New code & FORTRAN wrappers added to take care of histogram labels
 * 'Union set' computation on the PIAF master.
 *
 * Revision 1.5  1996/04/09 13:30:46  dinofm
 * Some code related to the management of the PIAF server version has been
 * moved to HBOOK to remove an 'Unsatisfied reference' when linking with
 * PACKLIB alone.
 *
 * Revision 1.4  1996/04/02 21:42:52  thakulin
 * Remove CERNLIB_PZPUTFIX and make the new code controlled
 * by NewPiaf function instead.
 *
 * Revision 1.3  1996/03/15 10:34:12  dinofm
 * Imakefile has been modified to compile C modules on HP using the -Dextname
 * option (this allows to use cfortran.h wrappers).
 * Tcpiaf.c contains new functions to:
 *
 * 	extract useful info from the PIAF log (e.g. server version)
 * 	manage the PIAF server version
 * 	pack Cuts according to the new QP data structure
 *
 * Revision 1.2  1996/03/04 17:02:48  cernlib
 * Remove calls to fcntl on VMS
 *
 * Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
/*CMZ :          30/01/96  14.06.38  by  Timo Hakulinen*/
/*-- Author :*/

/* TCPAW's CSETUP() modified for client access to Piaf server */

#ifdef _WINDOWS
#include <Winsock.h>
#define UNIX
#define EWOULDBLOCK 35
#define sleep Sleep
#endif


#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_APOF77))
#define APOFTN
#endif
#if defined(CERNLIB_IBM)
#define IBM
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
#define IBMVM
#endif
#if (defined(CERNLIB_QXNO_SC))&&(!defined(CERNLIB_WINNT))
#define QXNO_SC
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_WINNT))
#define UNIX
#endif

#if defined(CERNLIB_TGV)
#define TGV
#endif
#if defined(CERNLIB_UCX)
#define UCX
#endif

#ifdef IBM
#include <manifest.h>
#include <bsdtypes.h>
#include <tcperrno.h>
#endif

#ifdef UNIX
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <pwd.h>
#include <limits.h>

#define stat_2   stat

extern int errno;
#endif

#ifdef vms
#include <descrip.h>
#include <file.h>
#include <unixio.h>
#include <stat.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#ifdef vms

#if !defined(TGV) && !defined(UCX)
#define TGV
#endif

#ifdef TGV

#if defined(CERNLIB_QMALPH)
#define chmod  DECC$CHMOD
#define close  DECC$CLOSE
#define fstat  DECC$FSTAT
#define getcwd DECC$GETCWD /* In readfs */
#define open   DECC$OPEN
#define read   DECC$READ
#define write  DECC$WRITE
#define sleep  DECC$SLEEP
#define stat_2 DECC$STAT
#endif
#if !defined(CERNLIB_QMALPH)
#define stat_2 stat
#endif

#if defined(CERNLIB_QCDEC)
#define __ALPHA
#endif

#if !defined(_DECC_V4_SOURCE) && !defined(__DECC_VER)
#define _DECC_V4_SOURCE
#include <multinet_root:[multinet.include.sys]types.h>
#define __TIME_T 1
#include <multinet_root:[multinet.include.sys]time.h>
#include <multinet_root:[multinet.include.sys]ioctl.h>
#include <multinet_root:[multinet.include.sys]socket.h>
#include <multinet_root:[multinet.include.netinet]in.h>
#include <multinet_root:[multinet.include.netinet]tcp.h>
#include <multinet_root:[multinet.include.arpa]inet.h>
#include <multinet_root:[multinet.include]errno.h>
#include <multinet_root:[multinet.include]netdb.h>
#undef _DECC_V4_SOURCE
#else
#include <multinet_root:[multinet.include.sys]types.h>
#define __TIME_T 1
#include <multinet_root:[multinet.include.sys]time.h>
#include <multinet_root:[multinet.include.sys]ioctl.h>
#include <multinet_root:[multinet.include.sys]socket.h>
#include <multinet_root:[multinet.include.netinet]in.h>
#include <multinet_root:[multinet.include.netinet]tcp.h>
#include <multinet_root:[multinet.include.arpa]inet.h>
#include <multinet_root:[multinet.include]errno.h>
#include <multinet_root:[multinet.include]netdb.h>
#endif
#endif

#ifdef UCX
#include <types.h>
#include <time.h>
#include <socket.h>
#include <in.h>
#include <tcp.h>
#include <inet.h>
#include <errno.h>
#include <netdb.h>
extern unsigned short htons( unsigned short );
#define socket_close   close
#define socket_errno   errno
#define socket_perror  perror
#define stat_2 stat
#endif

#else /* not VMS */

# ifdef sun
# define BSD_COMP /* to get BSD ioctl's in Solaris */
# endif
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <netinet/in.h>
#ifndef IBM
#ifndef _WINDOWS
#include <netinet/tcp.h>
#endif
#endif
#include <arpa/inet.h>
#include <netdb.h>
#define socket_close   close
#define socket_errno   errno
#ifndef _WINDOWS
#define socket_ioctl   ioctl
#else
#define socket_ioctl ioctlsocket
#endif
#define socket_perror  perror

#endif

#ifdef TGV
#define SOCK_T short
#else
#define SOCK_T int
#endif

#ifndef S_IRUSR
#define S_IRUSR 0400
#define S_IWUSR 0200
#endif

#ifndef PATH_MAX
#  define PATH_MAX  512
#endif

#define BUFFER_SIZE  8192
#if defined(IBM) || defined(vms)
/* is this strange number really necessary for some systems?
   - file systems really tend to like powers of two better... */
# define MAX_RECLEN   32000
#else
# define MAX_RECLEN   (32 * 1024)
#endif

typedef int   INTEGER;
typedef float REAL;
#ifndef vms
typedef void  SUBROUTINE;
#else
#define SUBROUTINE void
#endif

/* New stuff to deal with the new QP */
#include "ntuple/qp_cuts.h"
#include <cfortran/cfortran.h>
#include "ntuple/paw_interface.h"

/* New stuff to deal with nTuple character variables (labels) */
#include "ntuple/smap.h"
#include "ntuple/str.h"
#define LABEL_FILLER '\v'

/* The set that will be union of all label sets found by slaves */
static SMap UnionSet;

/* from KUIP */
extern char *fstrdup();
extern char *fstr0dup();
extern int   fstrlen();
extern int   fstrset();
extern char *strlower();
extern void  ku_piaf();
extern char *ku_prop();
extern char *ku_pros();

/* globals */
static REAL *rzxin;
static REAL *rzxout;
static char piafrc_name[PATH_MAX+1];

#ifdef IBM

/* translation tables from COMMTXT TXTLIB */
extern char asciitoebcdic[];
extern char ebcdictoascii[];

void xlate_code( buf, table, count )
     char *buf;
     char *table;
     int count;
{
  int i;
  for( i = 0; i < count; i++ )
    buf[i] = table[buf[i]];
}

#endif


/*
 * store the pointer to /RZCOUNT/ to update the I/O count
 */
#ifdef IBM
# define pzcxio_ PZCXIO
# pragma linkage(PZCXIO,FORTRAN)
#else
# ifdef QXNO_SC
#  define pzcxio_ pzcxio
# endif
# ifdef _WINDOWS
#  define pzcxio_ __stdcall PZCXIO
# endif
#endif
void pzcxio_( RZXIN, RZXOUT )
  REAL *RZXIN;
  REAL *RZXOUT;
{
  rzxin = RZXIN;
  rzxout = RZXOUT;
}


/*
 * set the nodelay option to minimize round-trip time */
void tcp_nodelay( sockfd, mode )
     int sockfd;
     int mode;
{
#ifdef TCP_NODELAY
  int on = mode;
  setsockopt((SOCK_T)sockfd, IPPROTO_TCP, TCP_NODELAY, (char*)&on, sizeof(on));
#endif
}

/*
 * use bigger buffers for data transfers to/from PIAF
 * 4.3BSD has a limit of ~52000 bytes (strange figure),
 * so set the buffers to 48 KB
 */
void tcp_setbufs( sockfd )
     int sockfd;
{
#ifdef SO_RCVBUF
  int siz = 48 * 1024;
  setsockopt((SOCK_T)sockfd, SOL_SOCKET, SO_RCVBUF, (char*)&siz, sizeof(siz));
  setsockopt((SOCK_T)sockfd, SOL_SOCKET, SO_SNDBUF, (char*)&siz, sizeof(siz));
#endif
}

#ifndef _WINDOWS
static int pz_setup
#else
int __stdcall PZSETUP
#endif
( p_ptr, p_len, c_ptr, c_len, s_ptr, s_len, v_ptr, v_len,
              u_ptr, u_len )
     char *p_ptr;
     int   p_len;
     char *c_ptr;
     int   c_len;
     char *s_ptr;
     int   s_len;
     char *v_ptr;
     int   v_len;
     char *u_ptr;
     int   u_len;
{
  char port[32];                /* name of /etc/services entry "piafserv" */
  char cluster[32];             /* name of server cluster "piaf" */
  char server[32];              /* name of frontend host "piaf1.cern.ch" */
  char version[32];             /* version tag for piaf server */
  FILE *piafrc;
  int try_prompt = 3;
  int do_prompt = 0;
  int upass = 1;
#ifdef UNIX
  struct passwd *pw = getpwuid( getuid() );
#endif
#if defined(__alpha) && defined(__osf__)
  unsigned long minus1 = 0xffffffffLL;
#else
  unsigned long minus1 = -1;
#endif
  char user_name[32];
  char pass_word[32];
  char user_pass[64];
  char new_user_pass[68];
  char srv_reply[80];
  struct servent *sp;
  struct hostent *hp;
  unsigned short sport;
  unsigned long saddr;
  int sockfd;
  int i, n;

  for( i = 0; i < p_len && p_ptr[i] != ' '; i++ )
    port[i] = p_ptr[i];
  port[i] = '\0';

  for( i = 0; i < c_len && c_ptr[i] != ' '; i++ )
    cluster[i] = c_ptr[i];
  cluster[i] = '\0';

  for( i = 0; i < s_len && s_ptr[i] != ' '; i++ )
    server[i] = s_ptr[i];
  server[i] = '\0';

  for( i = 0; i < v_len && v_ptr[i] != ' '; i++ )
    version[i] = v_ptr[i];
  version[i] = '\0';

  for( i = 0; i < u_len && u_ptr[i] != ' '; i++ )
    user_pass[i] = u_ptr[i];
  user_pass[i] = '\0';

  if (strlen(user_pass) == 0) {
    upass     = 0;
    do_prompt = 1;
  }

  if( (sp = getservbyname( port, "tcp" )) == NULL ) {
    if( (sport = atoi( port )) == 0 ) {
      if (!strcmp(port, "piafserv"))
        sport = 348;              /* piafserv */
      else
        sport = 358;              /* piafslave */
    }
    sport = htons( sport );
  }
  else {
    sport = sp->s_port;
  }

  if( (saddr = inet_addr( server )) == minus1 ) {
    if( (hp = gethostbyname( server )) == NULL ) {
      printf( " *** Cannot find %s in host table\n", server );
      return -1;
    }
    saddr = ((struct in_addr*)(hp->h_addr))->s_addr;
  }

#ifdef IBMVM
  sprintf( piafrc_name, "dot.%src.a0", cluster );
#endif

#ifdef _WINDOWS
  sprintf( piafrc_name, ".%src", cluster );
#else
 #ifdef UNIX
  sprintf( piafrc_name, "%s/.%src", pw->pw_dir, cluster );
 #endif
#endif

#ifdef vms
  sprintf( piafrc_name, "sys$login:.%src", cluster );
#endif

  /* when no user_pass given, try to read password info from rc file */
  if( !upass && (piafrc = fopen( piafrc_name, "r" )) != NULL ) {
    char line[80];
    if( fgets( line, (sizeof line), piafrc ) != NULL
       && sscanf( line, "%s %s", user_name, pass_word ) == 2 ) {
      do_prompt = 0;
    }
    fclose( piafrc );
#ifndef IBM
    chmod( piafrc_name, S_IRUSR|S_IWUSR );
#endif
  }

  while( try_prompt-- > 0 ) {
    /* prompt for username and password */
    if( do_prompt ) {
      char *user;
      char *pass;

      user = ku_pros( "Piaf username:", NULL );
      if( user == NULL || user[0] == '\0' )
        return -1;
      strcpy( user_name, user );

      pass = ku_prop( "Piaf password:" );
      if( pass == NULL || pass[0] == '\0' )
        return -1;
      strcpy( pass_word, pass );
    }

    while( 1 ) {                /* loop over reroute requests */
      struct sockaddr_in piafaddr;

      /* fill socket descriptors */
      memset( &piafaddr, 0, (sizeof piafaddr) );
      piafaddr.sin_family = AF_INET;
      piafaddr.sin_port = sport;
      piafaddr.sin_addr.s_addr = saddr;

      /* establish connection */
      if( (sockfd = socket( AF_INET, SOCK_STREAM, 0 )) == - 1 ) {
        printf( " *** Cannot create socket\n" );
        socket_perror( "socket()" );
        return -1;
      }
      else {
        int bufsz = BUFFER_SIZE;  /* use large buffers for better throughput */
        setsockopt( (SOCK_T)sockfd, SOL_SOCKET, SO_RCVBUF,
                   (char*)&bufsz, (sizeof bufsz) );
        setsockopt( (SOCK_T)sockfd, SOL_SOCKET, SO_SNDBUF,
                   (char*)&bufsz, (sizeof bufsz) );
      }
      if( connect( (SOCK_T)sockfd, (struct sockaddr*)&piafaddr,
                  (sizeof piafaddr) ) == - 1 ) {
        printf( " *** Cannot connect to %s\n", server );
        socket_perror( "connect()" );
        socket_close( (SOCK_T)sockfd );
        return -1;
      }

      if (!upass) {
         sprintf( user_pass, "%s %s", user_name, pass_word );
         n = strlen( user_pass );
#ifdef IBM
         xlate_code( user_pass, ebcdictoascii, n );
#endif
         for( i = 0; i < n; i++ )
           user_pass[i] = ~user_pass[i];
      }

      sprintf(new_user_pass, "new:%s", user_pass);
#ifdef IBM
      xlate_code(new_user_pass, ebcdictoascii, 4);
#endif
      n = strlen(new_user_pass);

      if( send( (SOCK_T)sockfd, new_user_pass, n + 1, 0 ) != n + 1 ) {
        printf( " *** Cannot send authentication\n" );
        shutdown( (SOCK_T)sockfd, 2 );
        socket_close( (SOCK_T)sockfd );
        return -1;
      }

      if( (n = recv((SOCK_T)sockfd, srv_reply, (sizeof srv_reply), 0)) <= 0) {
        printf( " *** Server does not answer\n" );
        shutdown( (SOCK_T)sockfd, 2 );
        socket_close( (SOCK_T)sockfd );
        return -1;
      }
#ifdef TGV
      if (strncmp(srv_reply,"%DEBUG",6) == 0) {
pass_debug:
         printf("DEBUG-VMS: %s\n",srv_reply);
         n = recv((SOCK_T)sockfd, srv_reply, (sizeof srv_reply), 0);
         if (!strstr(srv_reply,"Okay")) goto pass_debug;
         strcpy(srv_reply,"Okay");
      }
#endif
#ifdef IBM
      xlate_code( srv_reply, asciitoebcdic, n - 1 );
#endif

      if( strncmp( srv_reply, "Reroute:", 8 ) != 0 )
        break;

      /* reroute to a different server node */
      shutdown( (SOCK_T)sockfd, 2 );
      socket_close( (SOCK_T)sockfd );
      if( (saddr = inet_addr( srv_reply + 8 )) == minus1 ) {
        printf( " *** Invalid reroute reply: %s\n", srv_reply );
        return -1;
      }
    }
    if( strncmp( srv_reply, "Passwd: ", 8 ) != 0 )
      break;

    /* wrong user/password: prompt again */
    printf( " *** %s\n", srv_reply + 8 );
    shutdown( (SOCK_T)sockfd, 2 );
    socket_close( (SOCK_T)sockfd );
    do_prompt = 1;
  }
  if( strcmp( srv_reply, "Okay" ) != 0 ) {
    printf( " *** Server reply: %s\n", srv_reply );
    shutdown( (SOCK_T)sockfd, 2 );
    socket_close( (SOCK_T)sockfd );
    return -1;
  }
  /* when not set, return user_pass */
  if (!upass) {
     memset(u_ptr, ' ', u_len);
     n = strlen(user_pass);
     strncpy(u_ptr, user_pass, n);
  }

  /* send version tag */
  n = strlen( version );
#ifdef IBM
  xlate_code( version, ebcdictoascii, n );
#endif
  if( send( (SOCK_T)sockfd, version, n + 1, 0 ) != n + 1 ) {
    printf( " *** Cannot send version tag\n" );
    shutdown( (SOCK_T)sockfd, 2 );
    socket_close( (SOCK_T)sockfd );
    return -1;
  }

  tcp_nodelay( sockfd, 1 );
  tcp_setbufs( sockfd );

#if !defined(IBM) && !defined(SIOCATMARK)
  printf( "\
 *** This TCP/IP package has no proper support for Out-of-band messages.\n\
 *** Do not hit Control-C while the server is processing a command!!!\n" );
#endif

  return sockfd;
}


#ifdef vms
INTEGER pzsetup( p_dsc, c_dsc, s_dsc, v_dsc, u_dsc )
     struct dsc$descriptor_s *p_dsc;
     struct dsc$descriptor_s *c_dsc;
     struct dsc$descriptor_s *s_dsc;
     struct dsc$descriptor_s *v_dsc;
     struct dsc$descriptor_s *u_dsc;
{
  return pz_setup( p_dsc->dsc$a_pointer, p_dsc->dsc$w_length,
                   c_dsc->dsc$a_pointer, c_dsc->dsc$w_length,
                   s_dsc->dsc$a_pointer, s_dsc->dsc$w_length,
                   v_dsc->dsc$a_pointer, v_dsc->dsc$w_length,
                   u_dsc->dsc$a_pointer, u_dsc->dsc$w_length );
}
#else
# ifdef IBM
#  define pzsetup_ PZSETUP
#  pragma linkage(PZSETUP,FORTRAN)
# else
#  ifdef QXNO_SC
#   define pzsetup_ pzsetup
#  endif
# endif

#ifndef _WINDOWS
INTEGER pzsetup_( p_dsc, c_dsc, s_dsc, v_dsc, u_dsc,
                  p_len, c_len, s_len, v_len, u_len )
     char *p_dsc;
     char *c_dsc;
     char *s_dsc;
     char *v_dsc;
     char *u_dsc;
# ifdef IBM
     int  *p_len;
     int  *c_len;
     int  *s_len;
     int  *v_len;
     int  *u_len;
# else
#  ifdef APOFTN
     short *p_len;
     short *c_len;
     short *s_len;
     short *v_len;
     short *u_len;
#  else
     int p_len;
     int c_len;
     int s_len;
     int v_len;
     int u_len;
#  endif
# endif
{
# if defined(IBM) || defined(APOFTN)
  return pz_setup( p_dsc, *p_len, c_dsc, *c_len,
                   s_dsc, *s_len, v_dsc, *v_len,
                   u_dsc, *u_len );
# else
  return pz_setup( p_dsc, p_len, c_dsc, c_len,
                   s_dsc, s_len, v_dsc, v_len,
                   u_dsc, u_len );
# endif
}
#endif
#endif /* not _WINDOWS */

#ifdef UNIX
/*
 * Normalize the file path (expand '/../', '/./', and '//').
 */
static
char *norm_fname(char *fn)
{
  char *ss;
  for (ss = fn; (ss = strchr(ss,'/')) != (char *)0; ) {
    char *sx = (char *)0;
    if (ss[1] == '.') {
      if (ss[2] == '.' && ss[3] == '/') {
        sx = &ss[4];
        if (ss == fn) return (char *)0;
        for (; ss[-1] != '/'; ss--) ;
      }
      else if (ss[2] == '/') {
        sx = &ss[3];
        ss++;
      }
    }
    else if (ss[1] == '/') {
      sx = &ss[2];
      ss++;
    }
    if (sx) {
      if (!*sx) return (char *)0;
      memmove(ss,sx,strlen(sx) + 1);
      ss--;
    }
    else
      ss++;
  }
  return fn;
}
#endif

/*
 * Check if the file to be staged is a local file and if so,
 * ask the password for the local system so that the stager
 * running in piaf machine can do an ftp.
 */
#ifndef _WINDOWS
static void pf_local
#else
void __stdcall PFLOCL
#endif
( p_ptr, p_len, c_ptr, c_len, islocl, istat )
     char *p_ptr;
     int   p_len;
     char *c_ptr;
     int   c_len;
     INTEGER *islocl;
     INTEGER *istat;
{
  static char *ftp_pass, *ftp_name;
  char fname[PATH_MAX + 1];
  int i, j;
  size_t fsize = 0;
#ifndef IBM
#ifndef _WINDOWS
  struct stat sbuf;
#endif
#endif

#define MAX_NPROMPT   3

  *istat = 0;
  for (i = 0; i < c_len; i++) c_ptr[i] = ' ';
  /* FATMEN path? */
  if (p_ptr[0] == '/' && p_ptr[1] == '/') return;
  fname[0] = '\0';
#ifdef UNIX
  /* pathname relative to cwd? */
  if (p_ptr[0] != '/') {
    char cwd[PATH_MAX + 1];
    for (i = 0; i < p_len && p_ptr[i] != ' '; i++) ;
    if (!getcwd(cwd,sizeof cwd) || strlen(cwd) + i + 2 > sizeof fname) {
      printf(" *** Can't expand source file name\n");
      *istat = 1;
      return;
    }
    strcpy(fname,cwd);
    if (fname[strlen(fname) - 1] != '/') strcat(fname,"/");
  }
#endif
  for (i = strlen(fname), j = 0;
       j < p_len && i < sizeof fname - 1 && p_ptr[j] != ' '; i++, j++)
    fname[i] = p_ptr[j];
  fname[i] = '\0';
  /* force PAW to think the file is local with default size 200MB */
  if (*islocl)
    fsize = 200000000;
  else {
#if !defined(IBM) && !defined(_WINDOWS)
    if (stat_2(fname,&sbuf) == 0)
      fsize = sbuf.st_size;
    else
      return;
#else
    FILE *f = fopen(fname,"r");
    if (f)
      fclose(f);
    else
      return;
#endif
  }
#ifdef UNIX
  if (!norm_fname(fname)) {
    printf(" *** Can't normalise source file name\n");
    *istat = 1;
    return;
  }
#endif
  if (!ftp_name) {
    FILE *piafrc;
    int ret;

    if((piafrc = fopen( piafrc_name, "r" )) != NULL ) {
      char line[80], name[40], pass[40];
      /* first line is the piaf password */
      fgets( line, (sizeof line), piafrc );
      if( fgets( line, (sizeof line), piafrc ) != NULL &&
          (ret = sscanf( line, "%s %s", name, pass )) >= 1 && ret <= 2) {
        ftp_name = strdup(name);
        if (ret == 2)
          ftp_pass = strdup(pass);
      }
      fclose( piafrc );
#ifndef IBM
      chmod( piafrc_name, S_IRUSR|S_IWUSR );
#endif
    }
    if (!ftp_name) {
      char *name, *pass, ques[60];
      int nprompt;
#if defined(UNIX) && !defined(_WINDOWS)
      struct passwd *upw = getpwuid(getuid());
      sprintf(ques,"FTP username (<CR>=%s):",upw->pw_name);
#else
      sprintf(ques,"FTP username (no default):");
#endif
      nprompt = 0;

      reask_user:;

      nprompt++;
      name = ku_pros(ques,NULL);
      if (name == NULL || name[0] == '\0') {
#if defined(UNIX) && !defined(_WINDOWS)
        name = upw->pw_name;
#else
        if (nprompt <= MAX_NPROMPT)
          goto reask_user;
        else {
          *istat = 1;
          return;
        }
#endif
      }
#if defined(UNIX) && !defined(_WINDOWS)
      if (strcmp(name,upw->pw_name)) {
        struct passwd *pw = getpwnam(name);
        if (!pw) {
          printf(" *** User name %s does not exist\n",name);
          goto reask_user;
        }
      }
#endif
      pass = ku_prop("FTP password (<CR>=no password):");
      if (pass == NULL || pass[0] == '\0')
        ftp_pass = (char *)0;
      else
        ftp_pass = strdup(pass);
      ftp_name = strdup(name);
    }
  }
  if (strlen(ftp_name) + (ftp_pass ? strlen(ftp_pass) : 0) + 24
      > (size_t)c_len) {
    printf(" *** Combined length of user name and password too great\n");
    *istat = 1;
    return;
  }
  if (strlen(fname) < (size_t)p_len) {
    sprintf(p_ptr,"%s",fname);
    p_ptr[strlen(p_ptr)] = ' ';
  }
  sprintf(c_ptr,"%lu %s %s",
    (unsigned long)fsize,ftp_name,ftp_pass ? ftp_pass : "");
  c_ptr[strlen(c_ptr)] = ' ';
}

#ifndef _WINDOWS
#ifdef vms
SUBROUTINE pflocl( p_dsc, c_dsc, islocl, istat )
     struct dsc$descriptor_s *p_dsc;
     struct dsc$descriptor_s *c_dsc;
     INTEGER *islocl;
     INTEGER *istat;
{
  pf_local( p_dsc->dsc$a_pointer, p_dsc->dsc$w_length,
            c_dsc->dsc$a_pointer, c_dsc->dsc$w_length, islocl, istat );
}
#else
# ifdef IBM
#  define pflocl_ PFLOCL
#  pragma linkage(PFLOCL,FORTRAN)
# else
#  ifdef QXNO_SC
#   define pflocl_ pflocl
#  endif
# endif
SUBROUTINE pflocl_( p_dsc, c_dsc, islocl, istat, p_len, c_len )
     char *p_dsc;
     char *c_dsc;
     INTEGER *islocl;
     INTEGER *istat;
# ifdef IBM
     int  *p_len;
     int  *c_len;
# else
#  ifdef APOFTN
     short *p_len;
     short *c_len;
#  else
     int p_len;
     int c_len;
#  endif
# endif
{
# if defined(IBM) || defined(APOFTN)
  pf_local( p_dsc, *p_len, c_dsc, *c_len, islocl, istat );
# else
  pf_local( p_dsc, p_len, c_dsc, c_len, islocl, istat );
# endif
}
#endif
#endif /* _WINDOWS */


/*
 * Generate the target filename by removing the leading path
 * from the source filename.
 */
static
char *target_name( source )
     char *source;
{
  char *target = strcpy( malloc( strlen( source ) + 1), source );
  char *p;

  /* Unix case: remove stuff up to last / */
  if( (p = strrchr( target, '/' )) != NULL )
    strcpy( target, p + 1 );

  /* VMS case: remove stuff up to last ] or : until ; */
  if( (p = strrchr( target, ']' )) != NULL )
    strcpy( target, p + 1 );
  if( (p = strrchr( target, ':' )) != NULL )
    strcpy( target, p + 1 );
  if( (p = strrchr( target, ';' )) != NULL )
    *p = '\0';

  /* VM/CMS case: convert blanks to dots and remove stuff after second dot */
  while( (p = strchr( target, ' ' )) != NULL )
    *p = '.';
  while( strchr( target, '.' ) != (p = strrchr( target, '.' )) )
    *p = '\0';

  return target;
}


/*
 * receive exactly len bytes into buffer
 */
int recv_len( sockfd, buf, len )
     int sockfd;
     char *buf;
     int len;
{
  int nrecv, n;

  for( n = 0; n < len; n += nrecv ) {
    if( (nrecv = recv((SOCK_T)sockfd, buf + n, len - n, 0 )) < 0 ) {
      socket_perror( "recv_len" );
      return -1;
    }
  }
  *rzxin += len >> 2;           /* increment RZIODO count */
  return n;
}


/*
 * receive a buffer headed by the length indicator and trailed by a null byte
 */
int recv_buf( sockfd, buf, length )
     int sockfd;
     char *buf;
     int length;
{
  char header[2];

  if( recv_len( sockfd, header, (sizeof header) ) > 0 ) {
    int count = ((header[1] & 0xFF) << 8) + (header[0] & 0xFF);

    if( count > length ) {
      printf( " *** recv_buf: Record header exceeds buffer size\n" );
      return -1;
    }
    else if( count != 0 ) {
      if( recv_len( sockfd, buf, count ) < 0 ) {
        printf( " *** recv_buf: Cannot receive buffer\n" );
        return -1;
      }
      if( buf[count-1] != '\0' ) {
        printf( " *** recv_buf: Invalid trailer byte\n" );
        return -1;
      }
    }
    return count;
  }
  return -1;
}


/*
 * receive a string with ASCII --> EBCDIC translation on IBM
 */
static
int recv_str( sockfd, string, length )
     int sockfd;
     char *string;
     int length;
{
  int count = recv_buf( sockfd, string, length );
#ifdef IBM
  if( count > 1 ) {
    asciitoebcdic[10] = '\n';   /* something wrong with newline translation */
    xlate_code( string, asciitoebcdic, count - 1 );
  }
#endif
  return count;
}


/*
 * send complete buffer
 */
int send_len( sockfd, buffer, length )
     int sockfd;
     char *buffer;
     int length;
{
  int n, nsend;
  for( n = 0; n < length; n += nsend ) {
    int chunk = length - n;
    if( chunk > BUFFER_SIZE )
      chunk = BUFFER_SIZE;
    if( (nsend = send( (SOCK_T)sockfd, buffer + n, chunk, 0 )) < 0 ) {
      socket_perror( "send_len" );
      return -1;
    }
  }
  return n;
}


#ifndef UNIX

/*
 * CZGETA emulation: receive 80 bytes and send 4 bytes acknowledgement
 */
static
int recv_msg( sockfd, msg )
     int sockfd;
     char *msg;
{
  char line[80];

  if( recv_len( sockfd, line, (sizeof line) ) > 0
     && send_len( sockfd, "Okay", 4 ) > 0 ) {
    int len;
#ifdef IBM
    xlate_code( line, asciitoebcdic, (sizeof line) );
#endif
    len = fstrlen( line, (sizeof line) );
    line[len] = '\0';
    strcpy( msg, line );
    return 0;
  }
  return -1;
}

#endif


/*
 * CZPUTA emulation: send 80 bytes and receive 4 bytes acknowledgement
 */
static
int send_msg( sockfd, msg )
     int sockfd;
     char *msg;
{
  char line[80];
  char ackn[4];
  int len = strlen( msg );
  strncpy( line, msg, len );
  memset( line + len, ' ', (sizeof line) - len );
#ifdef IBM
  xlate_code( line, ebcdictoascii, (sizeof line) );
#endif
  if( send_len( sockfd, line, (sizeof line) ) > 0
     && recv_len( sockfd, ackn, (sizeof ackn) ) > 0 ) {
    return 0;
  }
  return -1;
}


#ifdef IBM
/*
 * CZPUTA emulation: send 80 bytes and receive 4 bytes acknowledgement
 *                   BUT do no ebcdictoascii translation.
 *                   Used to send inverted ascii authorization string.
 */
#define czputb CZPUTB
#pragma linkage(CZPUTB,FORTRAN)

SUBROUTINE czputb(iskin, msg, l_msg, istat)
     int  *iskin;
     char *msg;
     int  *l_msg;
     int  *istat;
{
  char line[80];
  char ackn[4];
  int len = *l_msg;
  int sockfd = *iskin;
  strncpy( line, msg, len );
  memset( line + len, ' ', (sizeof line) - len );

  /* only convert blanks */
  xlate_code(line + len, ebcdictoascii, (sizeof line) - len);

  if( send_len( sockfd, line, (sizeof line) ) > 0
     && recv_len( sockfd, ackn, (sizeof ackn) ) > 0 ) {
    *istat = 0;
    return;
  }
  *istat = -1;
  return;
}
#endif


/*
 * send a two byte record length indicator
 */
static
int send_reclen( sockfd, reclen )
     int sockfd;
     int reclen;
{
  char header[2];
  header[0] = reclen & 0xFF;
  header[1] = (reclen >> 8 ) & 0xFF;
  return send_len( sockfd, header, (sizeof header) );
}


/*
 * send two null bytes as end of file indicator
 */
int send_eof( sockfd )
     int sockfd;
{
  return send_reclen( sockfd, 0 );
}


/*
 * send a buffer with length header and trailing null byte
 */
int send_buf( sockfd, buffer, length )
     int sockfd;
     char *buffer;
     int length;
{
  if( send_reclen( sockfd, length + 1 ) > 0
     && send_len( sockfd, buffer, length ) > 0
     && send_len( sockfd, "\0", 1 ) > 0 ) {

    *rzxout += length >> 2;       /* increment RZIODO count */
    return length;

  }
  return -1;
}


/*
 * send a string with EBCDIC --> ASCII translation on IBM
 */
int send_str( sockfd, string )
     int sockfd;
     char *string;
{
  int length = strlen( string );
  int n;
#ifdef IBM
  xlate_code( string, ebcdictoascii, length );
#endif
  n = send_buf( sockfd, string, length );
#ifdef IBM
  xlate_code( string, asciitoebcdic, length );
#endif
  return n;
}


#ifdef IBM

/*
 * write buffer with calls to fwrite()
 */
static
int fwrite_buf( buf, bufsz, stream, reclen )
     char *buf;
     int bufsz;
     FILE *stream;
     int reclen;
{
  int nwrite;
  int n;
  for( n = 0; n < bufsz; n += nwrite ) {
    nwrite = bufsz - n;
    if( nwrite > reclen )
      nwrite = reclen;
    if( fwrite( buf + n, 1, nwrite, stream ) != nwrite )
      break;
  }
  return n;
}

#endif


/*
 * receive a remote file
 */
static
int pz_get( sockfd, source, target, binary, reclen)
     int sockfd;
     char *source;
     char *target;
     int binary;
     int reclen;
{
  int status = -1;
  int fildes = -1;
  FILE *stream = NULL;

#ifdef UNIX
  binary = 1;
  reclen = BUFFER_SIZE;
#else
  if( binary && reclen == 0 ) {
    /* ask Piaf server for RZ record length */
    char line[80];

    sprintf( line, "INQRZ:%s", source );
    if( send_msg( sockfd, line ) == 0
       && recv_msg( sockfd, line ) == 0 ) {

      reclen = atoi( line );
      if( reclen <= 0 ) {
        printf( " *** %s\n", line + 8 );
        return status;
      }
    }
  }
#endif

#if defined(IBM) || defined(vms)
  if( reclen > MAX_RECLEN ) {
    printf( " *** Maximum record length (%d bytes) exceeded",
           MAX_RECLEN );
    return status;
  }
#endif

#ifdef IBM
  if( binary ) {
    char format[64];
    sprintf( format, "wb,type=record,recfm=f,lrecl=%d", reclen );
    stream = fopen( target, format );
  }
  else if( reclen > 0 ) {
    char format[32];
    sprintf( format, "w,recfm=f,lrecl=%d", reclen );
    stream = fopen( target, format );
  }
  else {
    stream = fopen( target, "w" );
    reclen = BUFFER_SIZE;
  }
#endif

#ifdef vms
  if( binary ) {
    char mrs[16];
    sprintf( mrs, "mrs=%d", reclen );
    fildes = open( target, O_WRONLY|O_CREAT|O_TRUNC, 0644,
                  "rfm=fix", mrs, "fop=tef" );
  }
  else {
    if( (stream = fopen( target, "w", "rfm=stmlf", "fop=tef" )) != NULL ) {
      fildes = fileno( stream );
      binary = 1;
    }
    reclen = BUFFER_SIZE;
  }
#endif

#ifdef UNIX
  fildes = open( target, O_WRONLY|O_CREAT|O_TRUNC, 0644 );
# endif

  if( fildes == -1 && stream == NULL ) {
    printf( " *** Cannot open local file %s\n", target );
    perror( NULL );
    return status;
  }

  if( send_msg( sockfd, "GETA :" ) == 0 ) {
    int nbuf = BUFFER_SIZE / reclen;
    int bufsz = (nbuf > 1) ? reclen * nbuf : reclen;

    /* send the source file name */
    if( send_str( sockfd, source ) > 0 ) {
      char answer[PATH_MAX+1];

      if( recv_str( sockfd, answer, (sizeof answer) ) != 0 ) {
        /* problem on server side */
        printf( " *** Cannot open remote file %s\n", answer );
        status = 1;
      }
      else if( send_reclen( sockfd, bufsz ) > 0 ) {
        /* send maximum buffer size and receive file content */
        time_t t0 = time( NULL );
        int ntrans = 0;
        int nblock = 0;
        char nokay = 0;         /* handshake flag */
        char nhand = 127;       /* handshake frequency */
        send( (SOCK_T)sockfd, &nhand, 1, 0 );

        status = 0;
        while( 1 ) {
          char buf[MAX_RECLEN+1];
          int nrecv;
          time_t t1 = time( NULL );
          if( t1 - t0 >= 10 ) {
            printf( " ... %d bytes transferred\n", ntrans );
            t0 = t1;
          }

          if( binary ) {
            if( (nrecv = recv_buf( sockfd, buf, MAX_RECLEN + 1 )) > 0 ) {
#ifdef IBM
              if( nokay == 0
                 && fwrite_buf( buf, nrecv - 1, stream, reclen ) != nrecv - 1 )
{
                perror( "fwrite" );
                nokay = 1;
              }
#else /* VMS or UNIX */
              if( nokay == 0
                 && write( fildes, buf, nrecv - 1 ) != nrecv - 1 ) {
                perror( "write" );
                nokay = 1;
              }
#endif
            }
          }
          else if( (nrecv = recv_str( sockfd, buf, MAX_RECLEN + 1 )) > 0 ) {
            if( nokay == 0
               && fputs( buf, stream ) == EOF ) {
              perror( "fputs" );
              nokay = 1;
            }
          }

          if( nrecv == 0 ) {    /* end of file */
            break;
          }
          if( nrecv < 0 ) {     /* receive error */
            status = -1;
            break;
          }

          ntrans += nrecv - 1;

          /*
           * If there was a problem in writing to disk we can stop receiving
           * only after sending the negative handshake signal
           */
          if( ++nblock == nhand ) { /* send handshake */
            send( (SOCK_T)sockfd, &nokay, 1, 0 );
            if( nokay != 0 ) {
              status = 1;
              continue;         /* server still sends EoF record */
            }
            nblock = 0;
          }
        }
      }
    }
  }
  if( stream != NULL )
    fclose( stream );
  else if( fildes != -1 )
    close( fildes );

  if( status )
    printf( " *** File transfer failed\n" );
  return status;
}


/*
 * fill buffer with calls to fgets()
 */
static
int fgets_buf( buf, bufsz, stream )
     char *buf;
     int bufsz;
     FILE *stream;
{
  int n = 0;
  while( fgets( buf + n, bufsz - n, stream ) != NULL ) {
    n += strlen( buf + n );
    if( n >= bufsz - 1 )
      break;
  }
  return n;
}

/*
 * fill buffer with calls to read()
 */
static
int read_buf(fildes, buf, bufsz)
   int   fildes;
   char *buf;
   int   bufsz;
{
   int nb;
   int n = 0;
   while ((nb = read(fildes, buf + n, bufsz - n)) > 0) {
      n += nb;
      if (n >= bufsz)
         break;
   }
   if (nb == -1)
      return -1;
   return n;
}


#ifdef IBM

/*
 * fill buffer with calls to fread()
 */
static
int fread_buf( buf, bufsz, stream )
     char *buf;
     int bufsz;
     FILE *stream;
{
  int reclen = fread( buf, 1, bufsz, stream );
  if( reclen > 0 ) {
    int n;
    for( n = reclen; bufsz - n >= reclen; n += reclen ) {
      if( fread( buf + n, 1, reclen, stream ) != reclen )
        break;
    }
    return n;
  }
  return 0;
}

#endif


/*
 * send a file to Piaf server
 */
static
int pz_put( sockfd, source, target, binary )
     int sockfd;
     char *source;
     char *target;
     int binary;
{
  char msg[25];
  int status = -1;
  int fildes = -1;
  int fsize  = 0;
  FILE *stream = NULL;

#ifdef IBM

  if( binary ) {
    stream = fopen( source, "rb,type=record" );
  }
  else {
    stream = fopen( source, "r" );
  }

#else /* UNIX or VMS */

  struct stat  stbuf;

  fildes = open( source, O_RDONLY, 0 );
  binary = 1;

#endif

  if( fildes == -1 && stream == NULL ) {
    printf( " *** Cannot open local file %s\n", source );
    perror( NULL );
    return status;
  }

#ifndef IBM   /* UNIX or VMS */

  if (fstat(fildes, &stbuf) == 0)
     fsize = stbuf.st_size;

#endif

  sprintf(msg, "PUTA :%12d", fsize);

  if( send_msg( sockfd, msg ) == 0 ) {
    char *ssrc, *sbuf;
    int sret;

    if (NewPiaf()) {
      /*
       * Send the source and target file names.
       * Earlier only the target name was sent but
       * then if the target file given is a name of
       * a directory one couldn't make the obvious expansion
       * based on the dir name and the source file name
       * because paw side doesn't know that the target is
       * a directory.  We send both names in one string
       * so as not to break compatibility with old paw
       * versions.  Piaf-side knows about the difference
       * and can act correctly in that case.
       */
      ssrc = strlower(target_name(source));
      sbuf = malloc(strlen(target) + strlen(ssrc) + 2);
      strcpy(sbuf,target);
      strcat(sbuf," ");
      strcat(sbuf,ssrc);
      sret = send_str( sockfd, sbuf );
    }
    else
      sret = send_str( sockfd, target );
    if (sret > 0) {
      int bufsz = binary ? MAX_RECLEN : BUFFER_SIZE;
      char answer[PATH_MAX+1];

      if( recv_str( sockfd, answer, (sizeof answer) ) != 0 ) {
        /* problem on server side */
        printf( " *** Cannot open remote file %s\n", answer );
        status = 1;
      }
      else if( send_reclen( sockfd, bufsz ) > 0 ) {
        /* send file content */
        time_t t0 = time( NULL );
        int ntrans = 0;
        int nblock = 0;
        char nokay = 0;         /* handshake flag */
        char nhand = 127;       /* handshake frequency */
        send( (SOCK_T)sockfd, &nhand, 1, 0 );

        /* allow TCP to buffer outgoing packets */
        tcp_nodelay( sockfd, 0 );

        status = 0;
        while( status == 0 ) {
          char buf[MAX_RECLEN];
          int nread;
          time_t t1 = time( NULL );
          if( t1 - t0 >= 10 ) {
            printf( " ... %d bytes transferred\n", ntrans );
            t0 = t1;
          }

          if( binary ) {

#ifdef IBM
            if( (nread = fread_buf( buf, bufsz, stream )) > 0 ) {
              if( send_buf( sockfd, buf, nread ) < 0 ) {
                status = -1;
              }
            }
            else if( ferror( stream ) ) {
              perror( "fread" );
              status = -1;
            }
            else                /* End of File */
              break;

#else /* VMS or UNIX */
            if( (nread = read_buf( fildes, buf, bufsz )) > 0 ) {
              if( send_buf( sockfd, buf, nread ) < 0 ) {
                status = -1;
              }
            }
            else if( nread == -1 ) {
              perror( "read" );
              status = -1;
            }
            else                /* End of File */
              break;
#endif
          }
          else if( (nread = fgets_buf( buf, BUFFER_SIZE, stream )) > 0 ) {
            if( send_str( sockfd, buf ) < 0 ) {
              status = -1;
            }
          }
          else if( ferror( stream ) ) {
            perror( "fgets" );
            status = -1;
          }
          else                  /* End of File */
            break;

          ntrans += nread;

          /* receive handshake flag and stop sending if server had a problem */
          if( ++nblock == nhand ) {
            nblock = 0;
            recv( (SOCK_T)sockfd, &nokay, 1, 0 );
            if( nokay != 0 ) {
              status = 1;
            }
          }
        }

        tcp_nodelay( sockfd, 1 );
        send_eof( sockfd );
      }
    }
    if (NewPiaf()) {
      free(ssrc);
      free(sbuf);
    }
  }

  if( stream != NULL )
    fclose( stream );
  if( fildes != -1 )
    close( fildes );

  if( status )
    printf( " *** File transfer failed\n" );
  return status;
}


/*
 * Transfer a file between client and Piaf server
 */
#ifndef _WINDOWS

#ifdef vms
INTEGER pzxfer( s_dsc, t_dsc, o_dsc, iskin, lrecl )
     struct dsc$descriptor_s *s_dsc;
     struct dsc$descriptor_s *t_dsc;
     struct dsc$descriptor_s *o_dsc;
     INTEGER *iskin;
     INTEGER *lrecl;
{
  char *s_ptr = s_dsc->dsc$a_pointer;
  int   s_len = s_dsc->dsc$w_length;
  char *t_ptr = t_dsc->dsc$a_pointer;
  int   t_len = t_dsc->dsc$w_length;
  char *o_ptr = o_dsc->dsc$a_pointer;
  int   o_len = o_dsc->dsc$w_length;
#else

# ifdef IBM
#  pragma linkage(PZXFER,FORTRAN)
INTEGER PZXFER( s_ptr, t_ptr, o_ptr, iskin, lrecl, s_dsc, t_dsc, o_dsc,
               i_dsc, l_dsc )
     char *s_ptr;
     char *t_ptr;
     char *o_ptr;
     INTEGER *iskin;
     INTEGER *lrecl;
     int  *s_dsc;
     int  *t_dsc;
     int  *o_dsc;
     int  *i_dsc;
     int  *l_dsc;
{
  int s_len = *s_dsc;
  int t_len = *t_dsc;
  int o_len = *o_dsc;
# else
#  ifdef APOFTN
INTEGER pzxfer( s_ptr, t_ptr, o_ptr, iskin, lrecl, s_dsc, t_dsc, o_dsc )
     char *s_ptr;
     char *t_ptr;
     char *o_ptr;
     INTEGER *iskin;
     INTEGER *lrecl;
     short *s_dsc;
     short *t_dsc;
     short *o_dsc;
{
  int s_len = *s_dsc;
  int t_len = *t_dsc;
  int o_len = *o_dsc;
#  else

#   ifdef QXNO_SC
#    define pzxfer_ pzxfer
#   endif
INTEGER pzxfer_( s_ptr, t_ptr, o_ptr, iskin, lrecl, s_len, t_len, o_len )
     char *s_ptr;
     char *t_ptr;
     char *o_ptr;
     INTEGER *iskin;
     INTEGER *lrecl;
     int s_len;
     int t_len;
     int o_len;
{
#  endif
# endif
#endif
#else /* _WINDOWS */
INTEGER __stdcall PZXFER(char *s_ptr, int s_len,
                         char *t_ptr, int t_len,
                         char *o_ptr, int o_len,
                         INTEGER *iskin,
                         INTEGER *lrecl)
{
#endif

  char *source = strlower( fstrdup( s_ptr, s_len ) );
  char *target = fstr0dup( t_ptr, t_len );
  char *chopt = fstrdup( o_ptr, o_len );
  int sockfd = *iskin;
  int reclen = *lrecl;
  int binary = (chopt[1] == 'B' );
  int status;

  if( target == NULL ) {
    target = strlower( target_name( source ) );
    fstrset( t_ptr, t_len, target );
  }
  else {
    strlower( target );
  }

  if( chopt[0] == 'G' ) {
    status = pz_get( sockfd, source, target, binary, reclen );
  }
  else {
    status = pz_put( sockfd, source, target, binary );
  }

  free( source );
  free( target );
  free( chopt );
  return status;
}

void
set_nonblock(fd)
int fd;
{
#ifndef vms
  int val = fcntl(fd,F_GETFL,0);
  val |= O_NONBLOCK;
  fcntl(fd,F_SETFL,val);
  errno = 0;
#endif
}

void
clr_nonblock(fd)
int fd;
{
#ifndef vms
  int val = fcntl(fd,F_GETFL,0);
  val &= ~O_NONBLOCK;
  fcntl(fd,F_SETFL,val);
  errno = 0;
#endif
}

int
oob_timeout(nloop)
int *nloop;
{
  struct timeval delay;
  delay.tv_sec = 0;
  delay.tv_usec = 100000;
  select( 0, NULL, NULL, NULL, &delay );
  if( ++*nloop > 100 )   /* 10 seconds timeout */
    return 1;
  return 0;
}

/*
 * called from the signal handler to synchronize with the Piaf server
 * after a ^C interrupt or to tell about a soft interrupt
 */
void piaf_sync( sockfd, oob )
     int sockfd;
     char *oob;
{
#if !defined(IBM) && defined(SIOCATMARK)
  int slave = (oob[0] & 248)>>3;  /* first five bits contain piaf slave id */

  char oobc = oob[0] & 7;  /* last three bits contain oob message type */

  /* send one byte out-of-band message to Piaf server */
  if( send( (SOCK_T)sockfd, &oobc, 1, MSG_OOB ) != 1 )
    return;

  if ( oobc == '\3' ) {        /* keyboard interrupt */
    char waste[1024];
    char oob_byte;
    int nbytes = 0;
    int nrecv;
    int nloop = 0;

    while( recv( (SOCK_T)sockfd, &oob_byte, 1, MSG_OOB ) < 0 ) {

      /*
       * If the OOB data hasn't arrived yet, we get either EWOULDBLOCK
       * or EINVAL.  The latter tries to tell us that it's not about
       * to arrive either.
       */
      if( socket_errno == EWOULDBLOCK ) {
        /*
         * In some systems (Solaris) regular recv() does not return upon
         * receiving the oob byte, which makes the below call to recv()
         * block indefinitely if there are no other data in the queue.
         * Use non-blocking mode to see if there is anything to flush.
         * We used to use FIONREAD ioctl, but it caused other problems
         * due to kernel bugs.  Besides non-blocking I/O is POSIX whereas
         * FIONREAD is just old 4.3BSD.
         */
        set_nonblock(sockfd);
        nrecv = recv( (SOCK_T)sockfd, waste, (sizeof waste), 0 );
        if (nrecv <= 0 && socket_errno != EWOULDBLOCK &&
            socket_errno != EAGAIN) {
          socket_perror( "piafsync: recv(waste)" );
          clr_nonblock(sockfd);
          return;
        }
        clr_nonblock(sockfd);
        if (nrecv > 0)
          nbytes += nrecv;
        if (oob_timeout(&nloop)) {
          printf( "piafsync: server does not respond\n" );
          return;
        }
      }

      else if( socket_errno == EINVAL ) {
        if (oob_timeout(&nloop)) {
          printf( "piafsync: server does not respond\n" );
          return;
        }
      }

      else {
        socket_perror( "piafsync: recv(MSG_OOB)" );
        return;
      }
    }

    /*
     * continue flushing the input socket stream until the OOB mark is reached
     */
    while( 1 ) {
      int atmark;

#ifdef TGV
      if (socket_ioctl((SOCK_T)sockfd, SIOCATMARK, (char *)&atmark) == -1) {
#else
      if( socket_ioctl((SOCK_T)sockfd, SIOCATMARK, &atmark ) == -1 ) {
#endif
        socket_perror( "piafsync: ioctl(SIOCATMARK)" );
        return;
      }

      if( atmark ) {
        /* OOB message reached */
        break;
      }

      /* flush the input stream */
      set_nonblock(sockfd);
      nrecv = recv( (SOCK_T)sockfd, waste, (sizeof waste), 0 );
      if (nrecv <= 0 && socket_errno != EWOULDBLOCK &&
          socket_errno != EAGAIN) {
        socket_perror( "piafsync: recv(waste)" );
        clr_nonblock(sockfd);
        return;
      }
      clr_nonblock(sockfd);
      if (nrecv)
        nbytes += nrecv;
      if (oob_timeout(&nloop)) {
        printf( "piafsync: server does not respond\n" );
        return;
      }
    }

    if( nbytes > 0 ) {
      if (slave == 0)
         printf( " *** Piaf synchronized: %d bytes discarded\n", nbytes );
      else
         printf( " *** Slave %d synchronized: %d bytes discarded\n", slave,
                 nbytes );
    }
  }
#endif
}


#ifdef IBM
# define pzsync_ PZSYNC
# pragma linkage(PZSYNC,FORTRAN)
#else
# ifdef QXNO_SC
#  define pzsync_ pzsync
# endif
# ifdef _WINDOWS
#  define pzsync_ __stdcall PZSYNC
# endif
#endif
void pzsync_( iskin )
     INTEGER *iskin;
{
  int sockfd = *iskin;

  if( sockfd < 0 )
    ku_piaf( sockfd, NULL );
  else
    ku_piaf( sockfd, piaf_sync );
}

#endif



/* New QP stuff. Many functions have FORTRAN wrappers to allow calling from */
/* FORTRAN code */

int
NewPiaf () {
       /* The oldest Piaf has version 0 */
       return (GetPiafVersion() > 0);
}


/* Function to retrieve the type of  the required Cut */
int ccuttype (int ACut) {
	return ((int) qp_cut_type (ACut));
}
FCALLSCFUN1(INT,ccuttype,ICUTTYPE,icuttype,INT)


int
cmaxcuts ()
{
	return MAX_CUT_ID;
}
FCALLSCFUN0(INT,cmaxcuts,MAXCUTS,maxcuts)


/* Function to 'pack' a Cut in a string: the Cut is transformed back in the */
/* PAW statement that was used to create it. Such a statement is then passed*/
/* over the network to the PIAF master to reproduce locally the Cut */
void cpackcut (int ACut, char *CBuf, int *Istat) {
	char *expr;
	int AllowedExpr = 1;
	*Istat = 0;
	expr = qp_cut_expr(ACut);
	if (!NewPiaf() && (strlen (expr) == 1)) {
		char fch;
		fch = toupper (expr[0]);
		if ((fch == 'X') || (fch == 'G'))
			AllowedExpr = 0;
	}
	if (AllowedExpr) {
		sprintf (CBuf,"nt/cuts $%d %s",ACut,qp_cut_expr(ACut));
	} else {
		char tmp[128];
		sprintf(tmp,"Cut #%d: expression not allowed on PIAF\n",ACut);
		sf_report(tmp);
		*Istat = 1;
	}
}
FCALLSCSUB3(cpackcut,PACKCUT,packcut,INT,PSTRING,PINT)


/* Function to 'pack' a graphical Cut. The graphical Cut is transformed  */
/* in a PACUTS pseudo-statement having the Cut id, the no. of points and */
/* one/two expression(s) as argument. The Points coordinate are packed   */
/* in a separate buffer as strings representing float numbers. The pseudo */
/* statement and the coordinates buffer are then sent over the network to */
/* the Piaf master that will reproduce locally the graphical Cut */
void 
cpackgcut (int ACut, char *CBuf, int *L, char *Points, int *Istat) 
{
  int i;
  CutGraf *TheGCut;
  TheGCut = gcut_get_data (ACut);
  *Istat = 0;
  if (TheGCut->dim == 1) {
    if (strlen(TheGCut->exprx) <= 67 ) {
      sprintf (CBuf,"PACUTS%4d%3d%s",ACut,TheGCut->np,TheGCut->exprx);
      sprintf (Points,"%12.6g%12.6g",TheGCut->points[0][0],
	       TheGCut->points[1][0]);
      *L = strlen (Points);
    } else {
      sf_report( "Cut expression can't be longer than 67 character\n" );
      *Istat = 1;
    }
  } else {
    if (NewPiaf() || (TheGCut->np <= 20)) {
      if (strlen(TheGCut->exprx) + strlen(TheGCut->expry) <= 67 ) {
	sprintf (CBuf,"PACUTS%4d%3d%s%%%s",ACut,TheGCut->np,TheGCut->exprx,
		 TheGCut->expry);
	Points[0] = '\0';
	for (i = 0; i < TheGCut->np; i++) {
	  char tmp [32];
	  sprintf (tmp,"%12.6g%12.6g",TheGCut->points[i][0],
		   TheGCut->points[i][1]);
	  strcat (Points,tmp);
	}
	*L = strlen (Points);
      } else {
	sf_report( "Cut expression can't be longer than 67 character\n" );
	*Istat = 1;
      }
    } else {
      sf_report( "Graphical cut on old Piaf supports maximum 20 points\n");
      *Istat = 1;
    }
  }
}
FCALLSCSUB5(cpackgcut,PACKGCUT,packgcut,INT,PSTRING,PINT,PSTRING,PINT)


/* Function to allocate the SMap representing the set of labels that is */
/* the union of all sets computed by slaves */

extern void 
UnpackLabels( SMap st, String s, int size)
{
  int pos = 0;
  char filler_str[2];
  String buf;
  filler_str[0] = LABEL_FILLER;
  filler_str[1] = '\0';
  buf = str_alloc( QP_STR_MAX + 1 );
  while ( pos < size ) {
    strncpy( buf, &(s[pos]), QP_STR_MAX);
    buf [QP_STR_MAX] = '\0';
    /* puts terminator on the 1st filler char (if any) */
    strtok( buf, filler_str );
    smap_add( st, buf );
    pos += QP_STR_MAX;
  }
  str_del( buf );
}

extern void 
PackLabels( SMap st, int *pos, String s, int *size)
{
  int i,offset,maxi;
  String ptr;
  offset = 0;
  maxi = smap_entries( st );
  i = *pos;
  /* We expect *size being the size of s not including the terminator */
  memset( s, LABEL_FILLER , *size );
  s[*size] = '\0';
  while(((*size - offset - QP_STR_MAX ) >= 0) && (i < maxi )) {
    ptr = smap_get ( st, (const int) i);
    memcpy( &(s[offset]), ptr, strlen( ptr ));
    i++;
    offset += QP_STR_MAX;
  }
  *size = offset;
  *pos = i;
}


int
LabelsLen( SMap sm )
{
  return ( sm->fEntries * QP_STR_MAX );
}

void 
InitUnionSet( int size )
{
  UnionSet = smap_new( size );
}
FCALLSCSUB1(InitUnionSet,INITUS,initus,INT)


/* Function to release the SMap representing the set of labels that is */
/* the union of all sets computed by slaves */

void 
ReleaseUnionSet()
{
  smap_del( UnionSet );
}
FCALLSCSUB0(ReleaseUnionSet,RELUS,relus )


/* Function to add the labels in the input argument the Union set */

void 
AddUnionSet( String s, int size )
{
  UnpackLabels( UnionSet, s, size );
}
FCALLSCSUB2(AddUnionSet,ADDUS,addus,PSTRING,INT)


/* Function to compute the Union set length as a string */

int 
UnionSetLen()
{
  return LabelsLen( UnionSet );
}
FCALLSCFUN0(INT,UnionSetLen,LGETUS,lgetus)


/* Function to pack a piece of Union set in a string */

void 
GetUnionSet( String s, int *len, int *LabelCount )
{
  PackLabels( UnionSet, LabelCount, s, len );
}
FCALLSCSUB3(GetUnionSet,CGETUS,cgetus,PSTRING,PINT,PINT)


/* Function to manage the slave/master messaging when a slave detects     */
/* it has no events to process.                                           */
/* If labels are not used, the  PPROJ0 message is issued to allow the     */
/* master to synchronize with the other slaves. If labels are used, the   */
/* PLABEL message is sent instead  */

void
PiafNoEvt ( bool Labels )
{
  if ( !Labels ) {
    float min = FLT_MAX/100.;
    float max = FLT_MIN*100.;
    PFMINMAX ( min, max );
  } else {
    SMap LabelList;
    String str;
    int len = 32768, pos = 0;
    LabelList = smap_new( 512 );
    str = str_alloc( len + 1 );
    PackLabels( LabelList, &pos, str, &len );
    PFLABELS( str, len );
    str_del( str );
    smap_del( LabelList ); 
  }
}


