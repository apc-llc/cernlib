/*
 * $Id: tcpold.c,v 1.2 2005/04/18 15:23:09 mclareni Exp $
 *
 * $Log: tcpold.c,v $
 * Revision 1.2  2005/04/18 15:23:09  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
#if defined(CERNLIB_OLD)
/*N.B. Must define sequence TCPLOG if a log file is required, e.g.*/
/*#define LOGFILE "disk$dd:-ben.socks-serv.log"*/ /* VMS    */
/*#define LOGFILE "/user/brun/ben/serv.log"    */ /* Apollo */
/*#define LOGFILE "/h0/psh/zs.log"             */ /* OS9    */
/*#define LOGFILE "/var/log/serv.log"          */ /* example*/
 
#if defined(CERNLIB__DOC)
/*      TCP/IP PACKAGE FOR REMOTE-PAW AND SIMILAR APPLICATIONS.
 
                     Ben M. Segal / CERN-CN/SW
 
                        ben@cernvax.cern.ch
 
         ------------------------------------------------------
        | Version of:  Aug.30, 1991  (FOR CERN PROGRAM LIBRARY)|
         ------------------------------------------------------
 
*****   THIS VERSION: SecurID/ACE ADDED (Unicos6.0 Version)
                      Removed limit of 8-char passwords (from "getpass").
 
*****   LAST VERSION: VM/CMS CODE ADDED (Ignacio Reguero / CERN-CN/SW)
 
*****   Documentation: "Installation and Use of the TCPAW Package",
                        Ben M. Segal / CERN-CN-SW   March 1991.
*/
#endif
/*      SYSTEM DEFINITIONS:                                                */
 
/* (VM, VMS, Apollo, HPUX, SGI, IBMRT, Sun, Cray, Ultrix, OS9 supported)   */
 
/*      CUSTOMIZATION DEFINITIONS:                                         */
#define DEBUG            /* Can remove from production versions            */
#define AUTHENT          /* INVOKES USER AUTHENTICATION CODE:  BUT NOTE... */
                         /* "AUTHENT" MUST MATCH IN CLIENTS AND SERVERS !! */
#define APOPAL           /* (use this to select OPAL OS-9 special login)   */
/*#define APOPWD */      /* (use if Unix password crypt fails on Apollos)  */
/*#define APOSR9 */      /* (used only for Apollo FTN linking pre-SR10)    */
/*#define SERVLOG*/      /* (use to get a server logfile: NOT IN CLIENT!!  */
/*#define SOCKETS*/      /* (optional: mainly for testing)                 */
/*#define XDEBUG */      /* (optional: BUT DON'T USE FOR INETD SERVERS)    */
/*#define CRAYC  */      /* (used only for testing from C on Crays)        */
/*#define RESOLVE_VIA_LOOKUP */  /* VM: to force use of a hosts table only */
/*#define NORUSERPASS */ /* (only to force use of std "ruserpass/getpass") */
/*      END OF DEFINITIONS                                                 */
 
#if defined(CERNLIB_IBMRT)
#define IBMRT
#endif
#if defined(CERNLIB_HPUX)
#define HPUX
#endif
#if defined(CERNLIB_SUN4)
#define sun4
#endif
#if defined(CERNLIB_OS9)
#define OSK
#endif
#if defined(CERNLIB_SGI)||defined(CERNLIB_MIPS)
#define sgi
#endif
#if defined(CERNLIB_IBMVM)
#define VM
#endif
#if (defined(CERNLIB_CRAY))&&(defined(CERNLIB_UNIX))&&(defined(CERNLIB_ACE))
#define ACE
#endif
 
#ifdef  VM
#include <tcpsock.h>
#if defined(CERNLIB_IBMVM)
#pragma csect(CODE,"TCPAWC")
#pragma linkage(cinit,FORTRAN)
#pragma linkage(isetup,FORTRAN)
#pragma linkage(iclose,FORTRAN)
#pragma linkage(csetup,FORTRAN)
#pragma linkage(ssetup,FORTRAN)
#pragma linkage(sclose,FORTRAN)
#pragma linkage(srecv,FORTRAN)
#pragma linkage(ssend,FORTRAN)
#pragma linkage(SSENSTR,FORTRAN)
#pragma linkage(SRECSTR,FORTRAN)
#pragma linkage(CMXLATE,OS)
#pragma linkage(CHPAS,OS)
#pragma linkage(LNRD,OS)
#pragma linkage(LNRDPAS,OS)
#pragma linkage(GETUSR,OS)
#pragma linkage(GETUNIQ,OS)
#endif
#endif /* VM */
 
#ifdef cray
#ifndef CRAYC
#define CRAYFTN
#endif /* CRAYC */
#ifdef ACE
#include "/usr/include/ace.h"
#endif /* ACE */
#endif /* cray */
 
#ifdef sgi
#define NOINITGROUPS
#endif /* sgi */
 
#ifdef apollo
#ifdef APOPWD
#define APOPWD1
#endif /* APOPWD */
#ifdef APOPAL
#define APOPAL1
#endif /* APOPAL */
#ifdef APOSR9
#define APOFTN
#endif /* APOSR9 */
#endif /* apollo */
 
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <ctype.h>
 
#ifdef AUTHENT
#ifndef OSK
#ifndef VM
#include <pwd.h>
#endif /* ^VM */
#endif /* OSK */
#endif /* AUTHENT */
 
#ifdef CRAYFTN
#include <fortran.h>
#endif /* CRAYFTN */
 
#ifdef  VMS
#include <iodef.h>
#include <ssdef.h>
#include <climsgdef.h>
#include <descrip.h>
#include <types.h>
#include <socket.h>
#include <in.h>
#else
#ifdef sgi
#include <bsd/sys/types.h>
#include <bsd/sys/socket.h>
#include <bsd/netinet/in.h>
#else
#ifdef  OSK
#include <time.h>
#include <types.h>
#include <socket.h>
#include <in.h>
#else
#ifdef  VM
extern int CHPAS();
extern char *LNRD();
extern char *LNRDPAS();
extern char *GETUSR();
#define getlogin GETUSR
extern int GETUNIQ();
#define getpid GETUNIQ
#define index strchr
#define ENOENT 67
#define ECONNREFUSED 61
#include <manifest.h>
#include <stdlib.h>
#include <time.h>
#include <bsdtypes.h>
#include <socket.h>
#include <in.h>
#include <string.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif /* VM */
#endif /* OSK */
#endif /* sgi */
#endif /* VMS */
 
#ifdef sgi
#include <bsd/netdb.h>
#else
#include <netdb.h>
#endif /* sgi */
 
#ifdef NOMEMSET
#define memset(a,b,c) bzero(a,c)   /* in case "memset" unknown (BS) */
#endif /* NOMEMSET */
 
#ifdef DEBUG                   /* DEBUG IMPLIES SOCKETS SUPPORT...  */
#define SOCKETS
static char *Prog_Name = "tcpaw";               /* CUSTOMIZE THIS.. */
#endif /* DEBUG */
 
#ifdef DEBUG
#ifdef SERVLOG                 /* DEBUG PLUS SERVLOG --> LOGFILE .. */
#ifdef VMS
#include "cspack/tcplog.h"
/*#define LOGFILE "disk$dd:[ben.socks]serv.log" */  /* CUSTOMIZE THIS.. */
#endif /* VMS */
#ifdef apollo
#include "cspack/tcplog.h"
/*#define LOGFILE "/user/brun/ben/serv.log"     */  /* CUSTOMIZE THIS.. */
#endif /* apollo */
#ifdef OSK
#include "cspack/tcplog.h"
/*#define LOGFILE "/h0/psh/zs.log"              */  /* CUSTOMIZE THIS.. */
#endif /* OSK */
#ifndef LOGFILE
#include "cspack/tcplog.h"
/*#define LOGFILE "/var/log/serv.log"           */  /* CUSTOMIZE THIS.. */
#endif /* LOGFILE */
#endif /* SERVLOG */
#endif /* DEBUG */
 
#ifdef LOGFILE
static FILE *logfile;
#endif /* LOGFILE */
 
#ifdef XDEBUG
#ifdef LOGFILE
#define STDERR logfile
#else
#define STDERR stderr
#endif /* LOGFILE */
#endif /* XDEBUG */
 
/* One or two global variables...*/
 
   static char clnthost[30];  /* client remote host name string */
   static char servhost[30];  /* server remote host name string */
   static unsigned short clntpport;    /* client local  port number */
   static unsigned short servpport;    /* server remote port number */
   static long timevar;       /* contains time returned by timexx */
   static int  ls = 0;        /* The server listen socket descriptor */
 
   extern char *inet_ntoa();
#ifndef VM
   extern int errno;
#endif /* ^VM */
   extern char *getenv(), *cuserid();
   unsigned long inet_addr();
 
#ifndef OSK
#ifndef VM
   extern char *ctime();
#endif /* ^VM */
 
#ifndef VM
/*  Note: INETD_SOCK_SETUP and INETD_SOCK_CLOSE are not needed for VM  ****/
 
/*
 *  INETD_SOCK_SETUP  (or: "ISETUP")
 *
 *      This routine sets up the necessary stuff for a server program to be
 *      started by a BSD-style inet-daemon (also found on WIN/TCP in VMS),
 *      and returns a socket for network sends and receives.
 *
 *      THE INTEGER RETURN VALUE, IF NEGATIVE, INDICATES AN ERROR !!!
 *
 *      ** It also returns an input and output unit number as parameters **
 *
 *      ** MULTI-USER VERSION **
 *      ** WITH USER AUTHENTICATION **
 */
 
#ifdef VMS
struct netdisc {
        int size;
        char *ptr;
} inetd = {10, "SYS$INPUT:"};
 
#ifdef AUTHENT
        struct  passwd *my_pw;
#endif /* AUTHENT */
#endif /* VMS */
 
int isetup(in, out)
int *in;
int *out;
{
        int s;
 
#ifdef VMS
        int status;
        unsigned short Channel;
#endif /* VMS */
 
#ifdef AUTHENT
        unsigned char idbuf[100];
        char  usbuf[20], pwbuf[20];
        char *user = usbuf, *passwd = pwbuf;
        register int i, len;
#endif /* AUTHENT */
 
#ifdef SOCKETS
        struct hostent *hp;             /* host info for remote host */
        struct sockaddr_in peeraddr_in; /* for peer socket address */
        int    peerlen;
#endif /* SOCKETS */
 
#ifdef VMS
/* Note: with newer VMS C compiler, cannot open LOGFILE before sys$assign! */
 
        status = sys$assign(&inetd, &Channel, 0,0);   /* sys$input channel */
        if (status != SS$_NORMAL) {
            return(-2);
 }
        s = Channel;
#else
        s = 0;  /* STDIN for Unix servers started by inetd */
#endif /* VMS */
 
#ifdef LOGFILE
        while ((logfile = fopen(LOGFILE, "a")) == NULL) sleep(1);
#endif /* LOGFILE */
 
#ifdef SOCKETS
        memset ((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
        peerlen = sizeof(peeraddr_in);
        if (getpeername(s, &peeraddr_in, &peerlen) < 0) {
#ifdef LOGFILE
                fprintf(logfile, "%s: getpeername failed\n", Prog_Name);
#endif /* LOGFILE */
                goto errout1;
        }
 
        hp = gethostbyaddr((char *) &peeraddr_in.sin_addr,
             sizeof (struct in_addr), peeraddr_in.sin_family);
        if (hp == NULL) {
                strcpy(clnthost, inet_ntoa(peeraddr_in.sin_addr));
        } else {
                strcpy(clnthost, hp->h_name); /* save remote host name */
        }
        clntpport = ntohs(peeraddr_in.sin_port);
#endif /* SOCKETS */
#ifdef LOGFILE
  /* Log a startup message. */
        time (&timevar);
        fprintf(logfile, "%s: Started  from %s port %u at %s",
                Prog_Name, clnthost, clntpport, ctime(&timevar));
#endif /* LOGFILE */
 
#ifdef AUTHENT
  /* Read the first buffer from the client and check the
     user name and password (which we assume to be in the
     format %s %s).         */
 
        len = recv(s, idbuf, sizeof(idbuf), 0);
        if (len <= 0) {
            reply("Bad id receive.\n", NULL);
            goto errout1;
        }
        idbuf[len-1]='\0';   /* SAFETY !! */
        if (--len > 0) for (i=0; i<len; i++) idbuf[i] = ~idbuf[i];
        if (sscanf((char *)idbuf, "%s %s", user, passwd) != 2) {
            reply("Bad id record '%s'\n", idbuf);
            goto errout1;
        }
#ifdef VMS
        my_pw = getpwnam(cuserid(0));
#endif /* VMS */
        if (chpass(user, passwd) <= 0) {
            goto errout1;
        }
  /* Successful authentication....... */
  /* Note: logfile closed in chpass if successful */
 
#else
#ifdef LOGFILE
        fclose(logfile);
#endif /* LOGFILE */
#endif /* AUTHENT */
 
        *in = *out = s;
        return(s);  /* return the user socket descriptor */
 
errout1:
 
#ifdef LOGFILE
        fclose(logfile);
#endif /* LOGFILE */
        return(-1); /* return -1 in case of failure */
}
 
/*
 *  INETD_SOCK_CLOSE  (or: "ICLOSE")
 *
 *      This routine closes down an inet-daemon server socket.
 *
 */
 
int iclose(s)
int *s;
{
 
#ifdef AUTHENT
#ifndef VMS
#ifdef  HPUX
        setresuid(0,0,0); /* have to do this to allow an exit !!! */
#else
        seteuid(0);       /* have to do this to allow an exit !!! */
#endif /* HPUX */
#else
        setuic(my_pw->pw_uic);
#endif /* VMS */
#endif /* AUTHENT */
 
#ifdef LOGFILE
        time (&timevar);
        while ((logfile = fopen(LOGFILE, "a")) == NULL) sleep(1);
        fprintf(logfile, "%s: Finished from %s port %u at %s",
                Prog_Name, clnthost, clntpport, ctime(&timevar));
        fclose(logfile);
#endif /* LOGFILE */
 
        if (*s != -1)
#ifdef VMS
            sys$dassgn(*s);
#else
            close(*s);
#endif /* VMS */
}
#endif /* OSK */
#endif /* VM */
 
/*
 *  SERVER_SOCK_SETUP  (or: "SSETUP")
 *
 *      This routine sets up the necessary stuff on a standalone server.
 *      It sets up the listen socket, accepts a single-user connect,
 *      and returns the user socket for network sends and receives.
 *
 *      THE INTEGER RETURN VALUE, IF NEGATIVE, INDICATES AN ERROR !!!
 *
 *      ** It also returns an input and output unit number as parameters **
 *
 *      ** SINGLE-USER VERSION ONLY (AND CODE FOR SPECIFIED PORT FOR VM) **
 *      ** ALSO OS9 VERSION (REALLY AN INETD_SERVER_SOCK_SETUP !) - PSH  **
 */
 
#ifdef  OSK
int ssetup(isock, osock)      /* OS9 */
int *isock, *osock;
{
#ifdef AUTHENT
        unsigned char idbuf[100];
        char  usbuf[20], pwbuf[20];
        char *user = usbuf, *passwd = pwbuf;
        register int i, len;
 
        int s = 0;
 
#ifdef LOGFILE
        while ((logfile = fopen(LOGFILE, "a")) == NULL) sleep(1);
#endif /* LOGFILE */
  /* Read the first buffer from the client and check the
     user name and password (which we assume to be in the
     format %s %s).         */
 
        len = recv(s, idbuf, sizeof(idbuf), 0);
        if (len <= 0) {
            reply("Bad id receive.\n", NULL);
            goto errout1;
        }
        if (--len > 0) for (i=0; i<len; i++) idbuf[i] = ~idbuf[i];
        i = sscanf((char *)idbuf, "%s %s", user, passwd);
        if (i != 1 && i != 2) {   /* Allow any password, even null for OS9 */
            reply("Bad id record '%s'\n", idbuf);
            goto errout1;
        }
        if (chpass(user, passwd) <= 0) {
            goto errout1;
        }
  /* Successful authentication....... */
  /* Note: logfile closed in chpass if successful */
        goto allok;
 
errout1:
 
#ifdef LOGFILE
        fclose(logfile);
#endif /* LOGFILE */
        return(-1); /* return -1 in case of failure */
 
allok:
 
#endif /* AUTHENT */
 
        *isock = 0;
        *osock = 1;
        return(*isock);
}
#else /* OSK */
 
/*  The old-fashioned single-user server:  NOTE "AUTHENT" NOT SUPPORTED !!! */
/*  (i.e. it will not talk correctly to a client with "AUTHENT" defined)    */
/*
    THIS IS ALSO USED FOR VM, INVOKED BY THE REXEC SERVER. IN THIS CASE,
    AUTHENTICATION IS DONE BY THE REXEC SERVER BEFORE THIS SERVER STARTS.
*/
int ssetup(in, out, port)               /* VM */
int   *in, *out;
int   *port;                            /* VM */
{
        struct hostent *hp;             /* host info for remote host */
        struct servent *sp;             /* pointer to service information */
 
        struct sockaddr_in myaddr_in;   /* for local socket address */
        struct sockaddr_in peeraddr_in; /* for peer socket address */
 
        int s, addrlen;
        unsigned short sport = *port;   /* VM */
 
  /* clear out address structures */
 
        memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
        memset ((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
 
  /* Set up address structure for the listen socket. */
        myaddr_in.sin_family = AF_INET;
        myaddr_in.sin_addr.s_addr = INADDR_ANY;
 
  /* Find the information for the "example" server
   * in order to get the needed port number.
   * ...UNLESS PASSED A +VE PORT NUMBER TO USE (VM).
   */
        if (port == NULL || *port <= 0) {            /* VM */
           sp = getservbyname ("example", "tcp");
           if (sp == NULL) {
               fprintf(stderr, "%s: 'example' not in services file\n",
                               "server_sock_setup");
               exit(1);
           }
           myaddr_in.sin_port = sp->s_port;
 
        } else myaddr_in.sin_port = htons(sport);    /* VM */
 
  /* Create the listen socket. */
        ls = socket(AF_INET, SOCK_STREAM, 0);
        if (ls == -1) {
#ifdef VM
                tcperror("server_sock_setup");
#else
                perror("server_sock_setup");
#endif /* VM */
                fprintf(stderr, "%s: unable to create socket\n",
                                "server_sock_setup");
                exit(1);
        }
  /* Bind the listen address to the socket. */
        if (bind(ls, &myaddr_in, sizeof(struct sockaddr_in)) == -1) {
#ifdef VM
                tcperror("server_sock_setup");
#else
                perror("server_sock_setup");
#endif /* VM */
                fprintf(stderr, "%s: unable to bind address\n",
                                "server_sock_setup");
                exit(1);
        }
  /* Initiate the listen on the socket so remote users
   * can connect.  The listen backlog is set to 5, which
   * is the largest currently supported.
   */
        if (listen(ls, 5) == -1) {
#ifdef VM
                tcperror("server_sock_setup");
#else
                perror("server_sock_setup");
#endif /* VM */
                fprintf(stderr, "%s: unable to listen on socket\n",
                                "server_sock_setup");
                exit(1);
        }
        addrlen = sizeof(struct sockaddr_in);
        s = accept(ls, &peeraddr_in, &addrlen);
        if ( s == -1) {
                fprintf(stderr, "%s: accept error\n", "server_sock_setup");
                return(-1);
        }
        hp = gethostbyaddr ((char *) &peeraddr_in.sin_addr,
                sizeof (struct in_addr), peeraddr_in.sin_family);
        if (hp == NULL) {
                strcpy(clnthost, inet_ntoa(peeraddr_in.sin_addr));
        } else {
                strcpy(clnthost, hp->h_name);  /* save remote host name */
        }
  /* Log a startup message. */
        time(&timevar);
        clntpport = ntohs(peeraddr_in.sin_port);
        fprintf(stderr, "Started  from %s port %u at %s",
                         clnthost, clntpport, ctime(&timevar));
 
        *in = *out = s;
        return(s);  /* Return the user socket descriptor */
}
#endif /* OSK */
 
/*
 *      CLIENT_SOCK_SETUP  (or: "CSETUP")
 *
 *      This routine sets up the TCP client side so that it can use a remote
 *      "srvc" server.  It creates a connection and returns the socket.
 *
 *      THE INTEGER RETURN VALUE, IF NEGATIVE, INDICATES AN ERROR !!!
 *
 *      ** It also returns an input and output unit number as parameters **
 *
 *      The name of the remote host to which the client will connect is given
 *      as the first parameter to the routine.
 *
 */
 
static int junk = 0;
#define NSO  100                        /* VM */
static int svmode[NSO], sxsock[NSO];    /* VM */
#define MAGIC 0x12345678                /* VM */
#define VMTO 30      /* 30 sec timeout   * VM */
 
#ifdef VMS
int csetup(sysdes, in, out, srvdes)            /* VMS CASE... */
struct dsc$descriptor_s  *sysdes;
int   *in, *out;
struct dsc$descriptor_s  *srvdes;
#else
#ifdef CRAYFTN
int csetup(sysdes, in, out, srvdes)            /* CRAY CASE... */
_fcd sysdes;
int   *in, *out;
_fcd srvdes;
#else
#ifdef APOFTN
int csetup(system, in, out, srvc, s_ln, c_ln)  /* APOLLO (PRE-SR10) FTN... */
char  *system;
int   *in, *out;
char  *srvc;
short *s_ln, *c_ln;        /* dummy arguments */
#else
int csetup(system, in, out, srvc)              /* UNIX ETC... */
char  *system;
int   *in, *out;
char  *srvc;
#endif /* APOFTN */
#endif /* CRAYFTN */
#endif /* VMS */
 
{
 struct hostent *hp;             /* host info for remote host */
 struct servent *sp;             /* pointer to service information */
 struct sockaddr_in myaddr_in;   /* for local socket address */
 struct sockaddr_in peeraddr_in; /* for peer socket address */
 
 unsigned long addr;
 int s, addrlen;
 int sx, nn = 0;                     /* REXEC */
#ifndef VM
 char get[50], *getp = get;          /* REXEC */
#endif /* VM */
 char sys[30];
 register int i, j, k;
 register char *c;
 
 char portstr[20], *pt;              /* VM */
 int    vmode = 0;                   /* VM */
 unsigned short sport = 0, prt = 0;  /* VM */
 char uspass[50];
 char *user = NULL, *passwd = NULL;
#ifdef AUTHENT
 unsigned char idbuf[100];
#endif /* AUTHENT */
 
#ifdef VMS
 char *system, *srvc;
 unsigned short s_ln, c_ln;
 struct dsc$descriptor_s sd, id;     /* VM */
 unsigned long cliflags;             /* VM */
 char command[200];                  /* VM */
 int  status, cstatus;               /* VM */
 
 srvc   = srvdes->dsc$a_pointer;
 c_ln   = srvdes->dsc$w_length;
 system = sysdes->dsc$a_pointer;
 s_ln   = sysdes->dsc$w_length;
 j = c_ln;
 i = s_ln;
#else
#ifdef CRAYFTN
 char *system, *srvc;
 unsigned s_ln, c_ln;
 
 srvc   = _fcdtocp(srvdes);
 c_ln   = _fcdlen(srvdes);
 system = _fcdtocp(sysdes);
 s_ln   = _fcdlen(sysdes);
 j = c_ln;
 i = s_ln;
#else
#ifdef APOFTN
 j = *c_ln;
 i = *s_ln;
#else
#ifdef VM
 extern char asciitoebcdic[];    /* translation tables */
 extern char ebcdictoascii[];
 char buff[100];
 char *get;
 char *getp;
 char getbuf[50];                       /* REXEC */
 getp = getbuf;                          /* REXEC */
 get = getbuf;                           /* REXEC */
/* j = 80; */      /* (crude kludge) length of srvc in Fortran */
/* i = 80; */      /* (  ,,    ,,  ) length of system in Fortran */
 i = gtlnfstr(&system,1);          /* length of 1st arg (srvc) */
 j = gtlnfstr(&system,4);          /* length of 4th arg (system) */
#else
 j = strlen(srvc);
 i = strlen(system);
#endif /* VM */
#endif /* APOFTN */
#endif /* CRAYFTN */
#endif /* VMS */
 
 /* Terminate correctly the host & service strings passed by FTN..  */
        for (k=0; k<j; k++) if (srvc[k] == ' ') { srvc[k] = '\0'; break;}
        for (k=0; k<i; k++) if (system[k] == ' ') { system[k] = '\0'; break;}
 /* And map to lower case...                                        */
        c = srvc;   while (*c) { if (isupper(*c)) *c = tolower(*c); c++; }
        c = system; while (*c) { if (isupper(*c)) *c = tolower(*c); c++; }
 
/*DEBUG fprintf(stderr, "system '%s' service '%s'\n", system, srvc);*/
 
        if ( !strcmp((system + strlen(system) - 3),"(vm") ){
            *(system + strlen(system) - 3) = '\0';
            strcpy(servhost, system);
            fprintf(stderr,"VM system: '%s'\n",system);
            goto vmcase;
        }
 
/*DEBUG fprintf(stderr, "system '%s' service '%s'\n", system, srvc);*/
 
        strcpy(servhost, system);  /* Use our own storage when needed....  */
        *sys = '\0';
        if ((addr = inet_addr(servhost)) != -1) {   /* Permit IP addresses */
#ifdef VM
           sprintf(buff, "Is system '%s' running VM/CMS (y/n)? [n]: ",
                             system);
           if (((get = LNRD(buff)) != NULL) && (*get == 'y' || *get == 'Y')) {
                sprintf(buff, "Give VM system name [CERNVM]: ");
                get = LNRD(buff);
                if (*get == '\0') strcpy(sys, "cernvm");
           else {
                strcpy(sys, get);
           }
           get = getp;
#else
           fprintf(stderr, "Is system '%s' running VM/CMS (y/n)? [n]: ",
                               system);
           if ((fgets(get, sizeof(get), stdin) != NULL) &&
                                (*get == 'y' || *get == 'Y')) {
                fprintf(stderr, "Give VM system name [CERNVM]: ");
                fgets(get, sizeof(get), stdin);
                if (*get == '\n') strcpy(sys, "cernvm");
                else {
                     get[strlen(get)-1] = '\0';        /* convert newline  */
                     strcpy(sys, get);
                }
#endif /* VM */
                goto vmcase;
           }
        }
/* NOTE THAT THE FOLLOWING TESTS ON MACHINE NAMES ARE CERN-SPECIFIC !! */
 if (!strncmp(system, "cernvm", 6) || !strncmp(system, "lepics", 6) ||
     !strncmp(system, "crnvm", 5))
  goto vmcase;
/* NOTE THAT THE FOLLOWING TESTS ON MACHINE NAMES ARE CERN-SPECIFIC !! */
/*
        if (!strncmp(system, "cernvm", 6) || !strncmp(system, "lepics", 6) ||
            !strncmp(system, "crnvm", 5))
                goto vmcase;
*/
#ifdef AUTHENT
/* Get the user name/password info from ~/.netrc file for authentication   */
 
#ifdef APOPAL1
/***     DIRTY TEMPORARY FIX FOR APOLLO-TO-OS9 AUTHENTICATION FOR PSH ***/
        if (!strncmp(system, "o-o", 3)) user = passwd = "(any)"; else
#endif /* APOPAL1 */
 
        ruserpass(servhost, &user, &passwd);       /* get info from .netrc */
        sprintf(uspass, "%s %s", user, passwd);
#endif /* AUTHENT */
        goto vmagain;
 
vmcase:
 
/* VM (rexec) MODE FOLLOWS:                        */
 
        vmode++;
 
        gethostname(portstr, sizeof(portstr));
        for (pt=portstr; *pt ; pt++) prt += *pt;
#ifdef DEBUG
/*
        fprintf(stderr, "localhost = %s\n", portstr);
        fprintf(stderr, "prt = %d prt<<5 = %d\n", prt, (prt<<5));
        fprintf(stderr, "pid = %d (%x)\n", getpid(), getpid());
*/
#ifdef VMS
/*      fprintf(stderr, "time = %d\n", time(0));   */
#endif /* VMS */
#endif /* DEBUG */
/*      if (!sport)                  ** DEBUG **/
        sport = (prt << 5) | (getpid() & 0x3e0) + junk++;
#ifdef VMS
        sport += (time(0) & 0xfff); /* Add 0-4095 as VMS pid may not change */
#endif /* VMS */
        sprintf(portstr, "%d", sport);
        fprintf(stderr, "Remote host/port = %s/%s\n", system, portstr);
 
/* Now it's time to give VM an "rexec" command to start the server.. */
 
        sp = getservbyname("exec", "tcp");
        if (sp == NULL) {
           fprintf(stderr, "'exec' not found in services file\n");
           return(-1);
        }
        strcpy(get, system);    /* rexec modifies its host argument! */
        if (*sys == '\0') {
            register char *cs = sys;
 
            c = system;     /* 'sys'= 'system' without domain extension */
            while ((*c) && (*c != '.')) {
                *cs++ = *c++;
            }
            *cs = '\0';
        }
        sprintf(uspass, "%s %s %s", srvc, portstr, sys);
#ifdef DEBUG
/*
        fprintf(stderr, "Rexec-ing %s cmd '%s'\n", system, uspass);
*/
#endif /* DEBUG */
 
        sx = rexec(&getp, (int)sp->s_port, user, passwd, uspass, NULL);
        if (sx < 0) {
            fprintf(stderr, "Bad rexec return %d\n", sx);
            fprintf(stderr, "Probable cause:\n");
            fprintf(stderr, "   a) You are logged on already \n");
            fprintf(stderr, "   b) You are disconnected (e.g. GONE)\n");
            fprintf(stderr, "   c) Bad VM username or password\n");
            return(-1);
        }
/*  fprintf(stderr, "Successfully rexec-ed %s cmd '%s'\n", system, uspass); */
 
        fprintf(stderr, "%s: loading %s exec (%d sec timeout)...\n\n",
                             system, srvc, VMTO);
vmagain:
  /* clear out address structures */
        memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
        memset ((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
 
  /* Set up the peer address to which we will connect. */
        peeraddr_in.sin_family = AF_INET;
  /* Get the host information for the hostname that the
   * user passed in.
   */
 
        if ((addr = inet_addr(servhost)) != -1) {  /* Permit IP addresses */
                peeraddr_in.sin_addr.s_addr = addr;
        } else {
                hp = gethostbyname(servhost);
                if (hp == NULL) {
                    fprintf(stderr, "%s: %s not found in hosts file\n",
                            "client_sock_setup", servhost);
                    return(-1);
                }
                peeraddr_in.sin_addr.s_addr =
                    ((struct in_addr *)(hp->h_addr))->s_addr;
/*      bcopy(hp->h_addr, (caddr_t)&peeraddr_in.sin_addr, hp->h_length); */
        }
 
        if (!vmode) {
  /* Find the information for the requested server
   * in order to get the needed port number.
   */
            sp = getservbyname (srvc, "tcp");
            if (sp == NULL) {
                fprintf(stderr, "%s: '%s' not found in services file\n",
                                "client_sock_setup", srvc);
                return(-1);
            }
            peeraddr_in.sin_port = sp->s_port;
        } else {
            peeraddr_in.sin_port = htons(sport);    /* VM */
        }
 
  /* Create the socket. */
        s = socket(AF_INET, SOCK_STREAM, 0);
        if (s == -1) {
#ifdef VM
            tcperror("client_sock_setup");
#else
            perror("client_sock_setup");
#endif /* VM */
            fprintf(stderr, "%s: unable to create socket\n",
                "client_sock_setup");
            if (vmode) close(sx);
            return(-1);
        }
  /* Try to connect to the remote server at the address
   * which was just built into peeraddr.
   */
        if (connect(s, &peeraddr_in, sizeof(struct sockaddr_in)) == -1) {
            close(s);
            if (vmode) while (nn++ < VMTO) {
          /*  fprintf(stderr, "Retry %d\n", nn); */
              sleep(1);
              goto vmagain;
            }
#ifdef VM
            tcperror("client_sock_setup");
#else
            perror("client_sock_setup");
#endif /* VM */
            fprintf(stderr, "%s: unable to connect to remote\n",
                "client_sock_setup");
            if (vmode) close(sx);
            return(-1);
        }
 
        addrlen = sizeof(struct sockaddr_in);
        if (getsockname(s, &myaddr_in, &addrlen) == -1) {
#ifdef VM
            tcperror("client_sock_setup");
#else
            perror("client_sock_setup");
#endif /* VM */
            fprintf(stderr, "%s: unable to read socket address\n",
                   "client_sock_setup");
            clntpport = 0;
        } else
            clntpport = ntohs(myaddr_in.sin_port);
 
  /* Print out a startup message for the user. */
        time(&timevar);
        fprintf(stderr, "Connected to %s on TCP port %u at %s",
                servhost, clntpport, ctime(&timevar));
 
#ifdef AUTHENT
        if (!vmode) {
 /* Send out the user/password authentication record first... */
           j = strlen(uspass);
#ifdef VM
           CMXLATE(uspass,ebcdictoascii,j);
#endif /* VM */
           for (i=0; i<j; i++) uspass[i] = ~uspass[i];
           if (send(s, uspass, j+1, 0) <= 0) {
#ifdef VM
               tcperror("Error sending authorization");
#else
               perror("Error sending authorization");
#endif /* VM */
               goto badserv;
           }
 /* Read back a confirmation from server...                   */
           if ((i = recv(s, idbuf, sizeof(idbuf), 0)) <= 0) {
#ifdef VM
               tcperror("Error receiving authorization");
#else
               perror("Error receiving authorization");
#endif /* VM */
badserv:
               fprintf(stderr,"%s: Service '%s' not set up OK on host %s.\n",
                     "client_sock_setup", srvc, servhost);
               close(s);
               return(-1);
           }
           idbuf[i] = '\0';
#ifdef VM
           CMXLATE(idbuf,asciitoebcdic,i);
#endif /* VM */
           fprintf(stderr, "%s\n", idbuf);
        }
#endif /* AUTHENT */
 
 /* Remember the sx sockets per VM socket (even for VMS!) */
        if (vmode) {
              svmode[s%NSO] = MAGIC;
              sxsock[s%NSO] = sx;
        }  else
              svmode[s%NSO] = sxsock[s%NSO] = 0;
 
        *in = *out = s;
        return(s);      /* return the socket descriptor to FTN */
 
}
 
/*
 *  SOCK_CLOSE  (or: "SCLOSE")
 *
 *      This routine closes a client (or a single-client server) socket.
 *
 */
 
int sclose(sock)
int  *sock;
 
{
    close(*sock);            /* close the user socket */
#ifndef OSK
    if (ls > 0) close(ls);   /* ..and the listen socket if an active server */
#endif /* OSK */
 
    if (svmode[(*sock)%NSO] == MAGIC) {
       close(sxsock[(*sock)%NSO]);   /* ..and the sx socket if appropriate  */
       svmode[(*sock)%NSO] = sxsock[(*sock)%NSO] = 0;
    }
}
 
/*
 *  SOCK_SENDSTR  (or: "SSENDSTR")
 *
 *      This routine sends the contents of a character string down a socket.
 *
 *      Args:    socket, string address, string length.
 *               NOTE:   ROUTINE ATTEMPTS TO SEND EXACT INPUT LENGTH.
 *
 *      Returns: Number of bytes sent/error (to be checked by user).
 */
 
#ifdef VMS
int ssendstr(sock, bufdes, m)      /* VMS CASE... */
int *sock;
struct dsc$descriptor_s  *bufdes;
int *m;
#else
#ifdef CRAYFTN
int ssendstr(sock, bufdes, m)      /* CRAY CASE... */
int *sock;
_fcd bufdes;
int *m;
#else
int ssendstr(sock, buf, m)
int  *sock;
char *buf;
int  *m;
#endif /* CRAYFTN */
#endif /* VMS */
 
{
#ifdef VM
    extern char asciitoebcdic[];    /* translation tables */
    extern char ebcdictoascii[];
#endif /* VM */
    int s = *sock, n = *m, i, j, k = 0;
 
#ifdef VMS
    char *buf = bufdes->dsc$a_pointer;
#endif /* VMS */
#ifdef CRAYFTN
    char *buf = _fcdtocp(bufdes);
#endif /* CRAYFTN */
 
#ifdef VM
    CMXLATE(buf,ebcdictoascii,strlen(buf));
#endif /* VM */
retry:
    if ((i =send(s, buf+k, n-k, 0)) <= 0) {
#ifdef XDEBUG
         tcperror("send");
         fprintf(STDERR,
              "sock_sendstr: sock = %d ret = %d len = %d k = %d buf[0-7] = '",
                             s, i, n, k);
         for (j=0; j<8; j++)  fprintf(STDERR, "%c", buf[j]);
         fprintf(STDERR, "'\n");
         if (i = 0) { sleep(2); goto retry; }
#endif /* XDEBUG */
         return(i);
    }
    if (i < (n-k)) { k += i; goto retry; }
    return(n);
}
 
/*
 *  SOCK_RECVSTR  (or: SRECVSTR")
 *
 *      This routine receives the contents of a character string on a socket.
 *
 *      Args:    socket, string address, [maximum] string length.
 *      NEW:     IF INPUT LENGTH > 0 ROUTINE ATTEMPTS  TO GET THIS LENGTH.
 *               IF   ,,    ,,   < 0 ROUTINE GETS WHAT IT CAN (OLD STYLE).
 *
 *      Returns: Number of bytes received/error (to be checked by user).
 *
 */
 
#ifdef VMS
int srecvstr(sock, bufdes, m)      /* VMS CASE... */
int *sock;
struct dsc$descriptor_s  *bufdes;
int *m;
#else
#ifdef CRAYFTN
int srecvstr(sock, bufdes, m)      /* CRAY CASE... */
int *sock;
_fcd bufdes;
int *m;
#else
int srecvstr(sock, buf, m)
int  *sock;
char *buf;
int  *m;
#endif /* CRAYFTN */
#endif /* VMS */
 
{
#ifdef VM
    extern char asciitoebcdic[];    /* translation tables */
    extern char ebcdictoascii[];
#endif /* VM */
    int s = *sock, n = *m, i, j, k = 0;
 
#ifdef VMS
    char *buf = bufdes->dsc$a_pointer;
#endif /* VMS */
#ifdef CRAYFTN
    char *buf = _fcdtocp(bufdes);
#endif /* CRAYFTN */
 
    if (n < 0) n = -n;  /* Look for maximum number of bytes option... */
 
retry:
    if ((i = recv(s, buf+k, n-k, 0)) <= 0) {
#ifdef VM
        CMXLATE(buf+k,asciitoebcdic,i);
#endif /* VM */
#ifdef XDEBUG
        tcperror("recv");
        fprintf(STDERR,
             "sock_recvstr: sock = %d ret = %d len = %d k = %d buf[0-15] = '",
                            s, i, *m, k);
        for (j=0; j<16; j++)  fprintf(STDERR, "%c", buf[j]);
        fprintf(STDERR, "'\n");
#endif /* XDEBUG */
        return(i);
    }
#ifdef VM
    CMXLATE(buf+k,asciitoebcdic,i);
#endif /* VM */
    if (*m > 0) {       /* look for exact number of bytes option... */
        if (i < (n-k)) { k += i; goto retry; }
        return(n);
    } else
       return(i);
}
 
/*
 *  SOCK_SEND  (or: "SSEND")
 *
 *      This routine sends a binary block of data down a socket.
 *
 *      Args:    socket, block address, block length.
 *               NOTE:   ROUTINE ATTEMPTS TO SEND EXACT INPUT LENGTH.
 *
 *      Returns: Number of bytes sent/error (to be checked by user).
 *
 */
 
int ssend(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
    int s = *sock, n = *m, i, j, k = 0;
 
retry:
    if ((i = send(s, buf+k, n-k, 0)) <= 0) {
#ifdef XDEBUG
        fprintf(STDERR,
             "sock_send: sock = %d ret = %d len = %d k = %d buf[0-7] = '",
                         s, i, n, k);
        for (j=0; j<8; j++)  fprintf(STDERR, "%c", buf[j]);
        fprintf(STDERR, "'\n");
#endif /* XDEBUG */
        return(i);
    }
    if (i < (n-k)) { k += i; goto retry; }
    return(n);
}
 
/*
 *  SOCK_RECV  (or: "SRECV")
 *
 *      This routine receives a binary block of data from a socket.
 *
 *      Args:    socket, receive buffer address, [maximum] block length.
 *      NEW:     IF INPUT LENGTH > 0 ROUTINE ATTEMPTS  TO GET THIS LENGTH.
 *               IF   ,,    ,,   < 0 ROUTINE GETS WHAT IT CAN (OLD STYLE).
 *
 *      Returns: Number of bytes received/error (to be checked by user).
 *
 */
 
int srecv(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
    int s = *sock, n = *m, i, j, k = 0;
 
    if (n < 0) n = -n;  /* Look for maximum number of bytes option... */
 
retry:
    if ((i = recv(s, buf+k, n-k, 0)) <= 0) {
#ifdef XDEBUG
        fprintf(STDERR,
             "sock_recv: sock = %d ret = %d len = %d k = %d buf[0-7] = '",
                         s, i, *m, k);
        for (j=0; j<8; j++)  fprintf(STDERR, "%c", buf[j]);
        fprintf(STDERR, "'\n");
#endif /* XDEBUG */
        return(i);
    }
    if (*m > 0) {       /* look for exact number of bytes option... */
        if (i < (n-k)) { k += i; goto retry; }
        return(n);
    } else
        return(i);
}
 
reply(s1, s2, s3)
    char *s1, *s2, *s3;
{
#ifdef OSK
    char buff[100];
    int  osock = 1;
 
    sprintf(buff, s1, s2, s3);
    send(osock, buff, strlen(buff)+1, 0);
#else
    printf(s1, s2, s3);
    fflush(stdout);
#endif /* OSK */
 
#ifdef LOGFILE
    fprintf(logfile, s1, s2, s3);
#endif /* LOGFILE */
}
 
#ifdef AUTHENT
/* Code derived from ftpd to do name/passwd checking.. */
/* Note that printf's get pushed down the network to client... */
#ifdef VM
int chpass(user, pass)           /* For VM */
    char *user, *pass;
{
    char *topoint;
    topoint = user;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    topoint = pass;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    if (CHPAS(user,pass)){    /*if pw check no ok (done in assembler) */
      reply("Bad password for user %s.\n", user);
      return(-3);
    }
    reply("User %s accepted.\n", user);
#ifdef LOGFILE
    fclose(logfile);
#endif /* LOGFILE */
    return(1);
}
#else /* ^VM */
#ifndef VMS
int chpass(user, pass)           /* For Unix and other "normal" people */
    char *user, *pass;
{
 
#ifndef OSK
 
        char  buff[100];
#ifdef APOPWD1
/** TEMPORARY APOLLO KLUDGE WHILE "crypt" DOESN'T WORK.. **/
#include <sys/wait.h>
 
        union wait ret;
#endif /* APOPWD1 */
 
        char   *xpasswd, *crypt();
        struct passwd *pw;
#ifdef ACE
        struct acmdata acmd;     /* Unicos6.0 calling sequence used..  */
 
        acmd.acm_both = 0;
#endif /* ACE */
 
        pw = getpwnam(user);
        if (pw == NULL) {
                reply("User %s unknown.\n", user);
                return(-2);
        }
 
#ifdef APOPWD1
/** TEMPORARY APOLLO KLUDGE WHILE "crypt" DOESN'T WORK.. **/
#define KLUDGE "-c /com/date >/dev/null 2>/dev/null"
 
        sprintf(buff, "%s %s %s %s %s","/com/login",user,"-lp", pass,KLUDGE);
        ret.w_status = system(buff);
        if (ret.w_Retcode) {
#else
#ifdef ACE
        acmd.acm_mode = CKUSR;
        acmd.acm_user = pw->pw_name;
        acmd.acm_clearpc = "";
        if (!acmlg(&acmd)) {
/* The user has a SecurID card ......................                     */
            int i;
 
            acmd.acm_clearpc = pass;
            if ((i = acmlg(&acmd))) {
                reply("Bad passcode for user %s: ACE error %d.\n",
                                        pw->pw_name, i);
                return(-7);
            }
        } else  {  /* apply the normal Unix password check ...            */
#endif /* ACE */
            xpasswd = crypt(pass, pw->pw_passwd);
            /* The strcmp does not catch null passwords! */
            if (*pw->pw_passwd == '\0' || strcmp(xpasswd,pw->pw_passwd)) {
#endif /* APOPWD1 */
                reply("Bad password for user %s.\n", pw->pw_name);
                return(-3);
            }
#ifdef ACE
        }
#endif /* ACE */
#ifdef HPUX
        if (setresgid(0,pw->pw_gid,0)) {
#else
#ifdef IBMRT
        setgid(pw->pw_gid);
#endif /* IBMRT */
        if (setegid(pw->pw_gid)) {
#endif /* HPUX */
                reply("Can't setegid for user %s.\n", pw->pw_name);
                return(-4);
        }
#ifndef NOINITGROUPS
        if (initgroups(pw->pw_name, pw->pw_gid)) {
                reply("Can't initgroups for user %s.\n", pw->pw_name);
                return(-5);
        }
#endif  /* NOINITGROUPS */
        if (chdir(pw->pw_dir)) {
#ifdef HPUX
                setresuid(0,0,0);
#else
                seteuid(0);
#endif /* HPUX */
                reply("Can't set home directory '%s' for user %s.\n",
                                 pw->pw_dir, pw->pw_name);
                return(-6);
        }
#endif /* OSK */
 
/* SUCCESS.................................................. */
        reply("User %s accepted (in directory %s).\n",
                                 pw->pw_name, pw->pw_dir);
#ifdef LOGFILE
        fclose(logfile);       /* Do this while still root.. */
#endif /* LOGFILE */
#ifndef OSK
#ifdef HPUX
        setresuid(0,pw->pw_uid,0);
#else
#ifdef IBMRT
        setuid(pw->pw_uid);
#endif /* IBMRT */
        seteuid(pw->pw_uid);
#endif /* HPUX */
#endif /* OSK */
        return(1);
}
 
#else  /* VMS */
 
int chpass(user, pass)           /* For VMS only (yes, it's different) */
    char *user, *pass;
{
        char   *xpasswd, *crypt();
        struct passwd *pw;
 
        pw = getpwnam(user);
        if (pw == NULL) {
                reply("User %s unknown.\n", user);
                return(-2);
        } else {
                xpasswd = vmscrypt(user, pass, pw->pw_encrypt, pw->pw_salt);
                /* The strcmp does not catch null passwords! */
                if (*pw->pw_passwd == '\0' || strcmp(xpasswd,pw->pw_passwd)) {
                        reply("Bad password for user %s.\n", user);
                        return(-3);
                }
        }
        if (setegid(pw->pw_gid) < 0) {
                reply("Can't setegid for user %s.\n", user);
                return(-4);
        }
/*
        if (initgroups(pw->pw_name, pw->pw_gid) < 0) {
                reply("Can't initgroups for user %s.\n", user);
                return(-5);
        }
*/
 
/*
 * Beware: on VMS, chdir is effective only during image execution.
 * Otherwise, chdir is to be called in SUPER, EXEC or KERNEL mode.
 */
        if (chdir(pw->pw_dir)) {
                setuic(my_pw->pw_uic);
                reply("Can't set home directory for user %s.\n", user);
                return(-6);
        }
 
/* Success..                    */
        reply("User %s accepted.\n", user);
#ifdef LOGFILE
        fclose(logfile);       /* Do this while still root.. */
#endif /* LOGFILE */
        if (seteuid(pw->pw_uid) < 0)    {
                printf("Can't seteuid for user %s.\n", user);
                fflush(stdout);
                return(-7);
        }
        if (setuser(pw->pw_name) != 0)  {       /* Change user name     */
                printf("Can't setuser for user %s.\n", user);
                fflush(stdout);
                return(-8);
        }
        if (setacct(pw->pw_account) != 0) {     /* Change account       */
                printf("Can't setacct for user %s.\n", user);
                fflush(stdout);
                return(-9);
        }
        if (setpriv(pw->pw_priv) != 0)  {       /* Change privileges    */
                printf("Can't setpriv for user %s.\n", user);
                fflush(stdout);
                return(-10);
        }
 
        return(1);
}
 
#endif /* VMS */
#endif /* VM */
#endif /* AUTHENT */
 
/* Provide all of ruserpass.c unless specifically omitted */
 
#ifdef VMS
/* Code from Unix RTL missing from VMS.... */
/* "ruserpass.c" -- to obtain a user's remote name and password */
/* Version hacked for VMS (and Wollongong) compatibility. (BS)  */
/* A VMS FTN calling program MUST call "vaxc$crtl_init" for getenv TO WORK */
 
#include <stat.h>
#include "sysreq/sgtty.h"
 
        static char usr[30], pss[30], myname[30];
 
ruserpass(host, aname, apass)
        char *host, **aname, **apass;
{
        struct sgttyb   sgttyb_val;
 
        if (*aname == NULL || *apass == NULL)
                rnetrc(host, aname, apass);
        if (*aname == NULL) {
                char *c = myname;
 
                strcpy(myname, getenv("USER"));
                while (*c) { *c = tolower(*c); c++; }
                fprintf(stderr, "Name (%s:%s): ", host, myname);
                if (fgets(usr, sizeof(usr), stdin) == NULL) goto fatal;
                if (*usr == '\n') {
                        strcpy(usr, myname);
                } else
                        usr[strlen(usr)-1] = '\0'; /* convert newline  */
                *aname = usr;
        }
        if (*aname && *apass == NULL) {
        /* FHE library needed for this echo-manipulation on VMS.....   */
                if (gtty(0, &sgttyb_val)) {
                        perror("gtty");
                        goto fatal;
                }
                sgttyb_val.sg_flags |= TT$M_NOECHO;  /* turn off echo  */
                if (stty(0, &sgttyb_val)) {
                        perror("stty(0)");
                        goto fatal;
                }
                fprintf(stderr, "Password (%s:%s): ", host, *aname);
                if (fgets(pss, sizeof(pss), stdin) == NULL) goto fatal1;
                fprintf(stdout,"\n");
                sgttyb_val.sg_flags &= ~(TT$M_NOECHO); /* restore echo */
                if (stty(0, &sgttyb_val)) {
                        perror("stty");
                        goto fatal;
                }
                pss[strlen(pss)-1] = '\0';         /* convert newline  */
                *apass = pss;
        }
        return;
fatal1:
        sgttyb_val.sg_flags &= ~(TT$M_NOECHO);         /* restore echo */
        stty(0, &sgttyb_val);
fatal:
        fprintf(stderr, "Fatal input error.\n");
        exit(1);
}
 
rnetrc(host, aname, apass)
        char *host, **aname, **apass;
{
 
        static FILE *cfile;
        char rcline[100];
        char *hdir, buf[100], mach[30];
        struct stat stb;
 
        hdir = getenv("HOME");
        if (hdir == NULL)
                hdir = ".";
        sprintf(buf, "%sftplogin.", hdir); /* Using the TWG FTPLOGIN file */
        cfile = fopen(buf, "r");
        if (cfile == NULL) {
                if (errno != ENOENT)
                        perror(buf);
                return;
        }
 
/* Note that the format of the TWG "ftplogin" file is simpler than .netrc */
 
        while (fgets(rcline, sizeof(rcline), cfile) != NULL) {
 
                if (sscanf(rcline, "%s %s %s", mach, usr, pss) != 3
                    || strcmp(host, mach)) continue;
 
                *aname = usr;  /* now we have a user on the right machine */
 
                if (!strcmp(pss, "-")) break;
 
/* Have a hit and a "real" password: check mode/ownership & warn if bad.. */
/* (No good on VMS as no bits for group/world r/w/x are provided........)
#define RWXRWX  077
 
                if (fstat(fileno(cfile), &stb) >= 0
                            && (stb.st_mode & RWXRWX) != 0) {
                    fprintf(stderr,
                      "WARNING - file: %s insufficiently protected.\n", buf);
                    fprintf(stderr,
                      "        - Remove password or restrict its access.\n");
                    break;
                }
 ..(No good on VMS as no bits for group/world r/w/x are provided........) */
 
                *apass = pss;   /* all OK */
                break;
        }
        fclose(cfile);
}
#else /* ^VMS */
 
#ifndef NORUSERPASS
 
#define DEFAULT 1
#define LOGIN   2
#define PASSWD  3
#define NOTIFY  4
#define WRITE   5
#define YES     6
#define NO      7
#define COMMAND 8
#define FORCE   9
#define ID      10
#define MACHINE 11
 
static char tokval[100];
 
static struct toktab {
        char *tokstr;
        int tval;
} toktab[]= {
        "default",      DEFAULT,
        "login",        LOGIN,
        "password",     PASSWD,
        "notify",       NOTIFY,
        "write",        WRITE,
        "yes",          YES,
        "y",            YES,
        "no",           NO,
        "n",            NO,
        "command",      COMMAND,
        "force",        FORCE,
        "machine",      MACHINE,
        0,              0
};
 
#ifdef VM
static FILE *cfile;
 
ruserpass(host, aname, apass)
 char *host, **aname, **apass;
{
  char *myname;
  char buff[100];
 
 if (*aname == 0 || *apass == 0)
  rnetrc(host, aname, apass);
/*rintf(stderr,"host = %s aname = %s apass = %s\n",host,*aname,*apass);*/
 if (*aname == 0) {
  myname = getlogin();
  sprintf(buff, "Name (%s:%s): ", host, myname);
  *aname = LNRD(buff);
  if ((*aname)[0] == '\0')
   *aname = myname;
 }
 if (*aname && *apass == 0) {
  sprintf(buff, "Password (%s:%s): ", host, *aname);
  *apass = LNRDPAS(buff);
 }
}
 
static
rnetrc(host, aname, apass)
 char *host, **aname, **apass;
{
 char buf[BUFSIZ];
 int t;
 
 strcpy(buf, "DOT.NETRC.A0");
 cfile = fopen(buf, "r");
 if (cfile == NULL) {
  if (errno != ENOENT)
   perror(buf);
  return;
 }
next:
 while ((t = token())) switch(t) {
 
 case DEFAULT:
  (void) token();
  continue;
 
 case MACHINE:
  if (token() != ID || strcmp(host, tokval))
   continue;
  while ((t = token()) && t != MACHINE) switch(t) {
 
  case LOGIN:
   if (token())
    if (*aname == 0) {
     *aname = malloc(strlen(tokval) + 1);
     strcpy(*aname, tokval);
    } else {
     if (strcmp(*aname, tokval))
      goto next;
    }
   break;
  case PASSWD:
   if (token() && *apass == 0) {
    *apass = malloc(strlen(tokval) + 1);
    strcpy(*apass, tokval);
   }
   break;
  case COMMAND:
  case NOTIFY:
  case WRITE:
  case FORCE:
   (void) token();
   break;
  default:
 fprintf(stderr, "Unknown .netrc option %s\n", tokval);
   break;
  }
  goto done;
 }
done:
 fclose(cfile);
}
 
#else /* ^VM */
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
 
char    *malloc(), *index(), *getpass(), *getlogin();
static  FILE *cfile;
 
ruserpass(host, aname, apass)
        char *host, **aname, **apass;
{
 
        if (*aname == 0 || *apass == 0)
                rnetrc(host, aname, apass);
        if (*aname == 0) {
                char *myname = getlogin();
                *aname = malloc(16);
                printf("Name (%s:%s): ", host, myname);
                fflush(stdout);
                if (read(2, *aname, 16) <= 0)
                        exit(1);
                if ((*aname)[0] == '\n')
                        *aname = myname;
                else
                        if (index(*aname, '\n'))
                                *index(*aname, '\n') = 0;
        }
        if (*aname && *apass == 0) {
                printf("Password (%s:%s): ", host, *aname);
                fflush(stdout);
                *apass = getpass("");
        }
}
 
static
rnetrc(host, aname, apass)
        char *host, **aname, **apass;
{
        char *hdir, buf[BUFSIZ];
        int t;
        struct stat stb;
        extern int errno;
        char *getenv();
 
        hdir = getenv("HOME");
        if (hdir == NULL)
                hdir = ".";
        sprintf(buf, "%s/.netrc", hdir);
        cfile = fopen(buf, "r");
        if (cfile == NULL) {
                if (errno != ENOENT)
                        perror(buf);
                return;
        }
next:
        while ((t = token())) switch(t) {
 
        case DEFAULT:
                (void) token();
                continue;
 
        case MACHINE:
                if (token() != ID || strcmp(host, tokval))
                        continue;
                while ((t = token()) && t != MACHINE) switch(t) {
 
                case LOGIN:
                        if (token())
                                if (*aname == 0) {
                                        *aname = malloc(strlen(tokval) + 1);
                                        strcpy(*aname, tokval);
                                } else {
                                        if (strcmp(*aname, tokval))
                                                goto next;
                                }
                        break;
                case PASSWD:
                        if (fstat(fileno(cfile), &stb) >= 0
                            && (stb.st_mode & 077) != 0) {
        fprintf(stderr, "Error - .netrc file not correct mode.\n");
        fprintf(stderr, "Remove password or correct mode.\n");
                                exit(1);
                        }
                        if (token() && *apass == 0) {
                                *apass = malloc(strlen(tokval) + 1);
                                strcpy(*apass, tokval);
                        }
                        break;
                case COMMAND:
                case NOTIFY:
                case WRITE:
                case FORCE:
                        (void) token();
                        break;
                default:
        fprintf(stderr, "Unknown .netrc option %s\n", tokval);
                        break;
                }
                goto done;
        }
done:
        fclose(cfile);
}
 
#endif /* VM */
 
static
token()
{
        char *cp;
        int c;
        struct toktab *t;
 
        if (feof(cfile))
                return (0);
        while ((c = getc(cfile)) != EOF &&
            (c == '\n' || c == '\t' || c == ' ' || c == ','))
                continue;
        if (c == EOF)
                return (0);
        cp = tokval;
        if (c == '"') {
                while ((c = getc(cfile)) != EOF && c != '"') {
                        if (c == '\\')
                                c = getc(cfile);
                        *cp++ = c;
                }
        } else {
                *cp++ = c;
                while ((c = getc(cfile)) != EOF
                    && c != '\n' && c != '\t' && c != ' ' && c != ',') {
                        if (c == '\\')
                                c = getc(cfile);
                        *cp++ = c;
                }
        }
        *cp = 0;
        if (tokval[0] == 0)
                return (0);
        for (t = toktab; t->tokstr; t++)
                if (!strcmp(t->tokstr, tokval))
                        return (t->tval);
        return (ID);
}
 
#ifndef VM
/* I provide "getpass" myself as standard version truncates to 8 characters */
#include <sys/ioctl.h>
#ifdef CBREAK
#define BSDTTY         /* First find out if BSD or SYSV terminal handling.. */
#endif
 
#ifndef BSDTTY
#include <termio.h>
#else
#include <sgtty.h>
#endif
 
extern int kill(), ioctl(), getpid();
static int intrupt;
 
/*** NOTE MAXPASSWD IS DEFINED AS 8 IN ALL STANDARD UNIX SYSTEMS, BUT THIS
 *** GIVES US PROBLEMS INTERWORKING WITH VMS AND CRAY-SECURID SYSTEMS. ***/
#define MAXPASSWD     20       /* max significant characters in password */
 
char *
getpass(prompt)
char    *prompt;
{
#ifndef BSDTTY
        struct termio ttyb;
        unsigned short flags;
#else
        struct sgttyb ttyb;
        int flags;
#endif
        register char *p;
        register int c;
        FILE    *fi;
        static char pbuf[ MAXPASSWD + 1 ];
        void    (*sig)(), catch();
 
        if((fi = fopen("/dev/tty", "r")) == NULL)
                return((char*)NULL);
        setbuf(fi, (char*)NULL);
        sig = signal(SIGINT, catch);
        intrupt = 0;
#ifndef BSDTTY
        (void) ioctl(fileno(fi), TCGETA, &ttyb);
        flags = ttyb.c_lflag;
        ttyb.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
        (void) ioctl(fileno(fi), TCSETAF, &ttyb);
#else
        gtty(fileno(fi), &ttyb);
        flags = ttyb.sg_flags;
        ttyb.sg_flags &= ~ECHO;
        stty(fileno(fi), &ttyb);
#endif
        (void) fputs(prompt, stderr);
        p = pbuf;
        while( !intrupt  &&
                (c = getc(fi)) != '\n'  &&  c != '\r'  &&  c != EOF ) {
                if(p < &pbuf[ MAXPASSWD ])
                        *p++ = c;
        }
        *p = '\0';
#ifndef BSDTTY
        ttyb.c_lflag = flags;
        (void) ioctl(fileno(fi), TCSETAW, &ttyb);
#else
        ttyb.sg_flags = flags;
        stty(fileno(fi), &ttyb);
#endif
        (void) putc('\n', stderr);
        (void) signal(SIGINT, sig);
        if(fi != stdin)
                (void) fclose(fi);
        if(intrupt)
                (void) kill(getpid(), SIGINT);
        return(pbuf);
}
 
static void
catch()
{
        ++intrupt;
}
#endif /* ^VM */
#endif /* NORUSERPASS */
#endif /* VMS */
 
/* I provide "rexec" myself as standard version does not allow IP addresses */
 
rexec(ahost, rport, name, pass, cmd, fd2p)
        char **ahost;
        int rport;
        char *name, *pass, *cmd;
        int *fd2p;
{
#ifdef VM
        extern char asciitoebcdic[];    /* translation tables */
        extern char ebcdictoascii[];
#endif /* VM */
        int s, timo = 1;
        struct sockaddr_in sin;
        char c;
        short port;
        struct hostent *hp;
        unsigned long addr;
 
        if ((addr = inet_addr(*ahost)) != -1) {  /* Permit IP addresses */
                sin.sin_addr.s_addr = addr;
        } else {
                hp = gethostbyname(*ahost);
                if (hp == 0) {
                       fprintf(stderr, "%s: unknown host\n", *ahost);
                       return (-1);
                }
/* NOT NICE     *ahost = hp->h_name;                   THIS IS NOT NICE */
        }
        ruserpass(*ahost, &name, &pass);
 
retry:
        s = socket(AF_INET, SOCK_STREAM, 0);
        if (s < 0) {
#ifdef VM
                tcperror("rexec: socket");
#else
                perror("rexec: socket");
#endif /* VM */
                return (-1);
        }
        sin.sin_family = AF_INET;
        sin.sin_port = (short)rport;
        if (addr != -1)                      {  /* Permit IP addresses */
                sin.sin_addr.s_addr = addr;
        } else {
                sin.sin_addr.s_addr =
                             ((struct in_addr *)(hp->h_addr))->s_addr;
/*              bcopy(hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length); */
        }
        if (connect(s, &sin, sizeof(struct sockaddr_in)) < 0) {
                if (errno == ECONNREFUSED && timo <= 16) {
                        (void) close(s);
                        sleep(timo);
                        timo *= 2;
                        goto retry;
                }
#ifdef VM
                tcperror("rexec: connect");
#else
                perror("rexec: connect");
#endif /* VM */
                return (-1);
        }
        if (fd2p == 0) {
                (void) send(s, "", 1, 0);
                port = 0;
        } else {
                fprintf(stderr, "Control channel not implemented\n");
                goto bad;
        }
#ifdef VM
        CMXLATE(name,ebcdictoascii,strlen(name));
        CMXLATE(pass,ebcdictoascii,strlen(pass));
        CMXLATE(cmd,ebcdictoascii,strlen(cmd));
#endif /* VM */
        (void) send(s, name, strlen(name) + 1, 0);
        (void) send(s, pass, strlen(pass) + 1, 0);
        (void) send(s, cmd, strlen(cmd) + 1, 0);
        if (recv(s, &c, 1, 0) != 1) {
#ifdef VM
                tcperror("rexec recv");
#else
                perror("rexec recv");
#endif /* VM */
                goto bad;
        }
        if (c != 0) {
                fprintf(stderr,"c = ");
                while (recv(s, &c, 1, 0) == 1) {
#ifdef VM
                     CMXLATE(&c,asciitoebcdic,1);
#endif /* VM */
                     fprintf(stderr,"%c",c);
                     (void) send(2, &c, 1, 0);
                     if (c == '\n')
                     break;
                }
                fprintf(stderr,"\n ended");
                goto bad;
        }
        return (s);
bad:
        if (port)
                (void) close(*fd2p);
        (void) close(s);
        return (-1);
}
 
 
/******** INTERFACE ROUTINES TO HANDLE VARIOUS NAMING CONVENTIONS *********/
 
/*** COMPATIBILITY-MODE: FULL-LENGTH NAMES AS USED IN EARLY "TCPAW"     ***/
 
#ifndef OSK
#ifndef VM
int inetd_sock_setup(in, out)
int  *in, *out;
{
   return(isetup(in, out));
}
 
int inetd_sock_close(sock)
int *sock;
{
   return(iclose(sock));
}
#endif /* ^VM */
#endif /* OSK */
 
int server_sock_setup(in, out)
int  *in, *out;
{
   return(ssetup(in, out));
}
 
#ifdef VMS
int client_sock_setup(sysdes, in, out, srvdes)            /* VMS CASE... */
struct dsc$descriptor_s  *sysdes;
int   *in, *out;
struct dsc$descriptor_s  *srvdes;
{
   return(csetup(sysdes, in, out, srvdes));
}
#else
#ifdef APOFTN
int client_sock_setup(system, in, out, srvc, s_ln, c_ln) /* APOLLO FTN CASE */
char  *system;                                           /* (PRE-SR9 ONLY)  */
int   *in, *out;
char  *srvc;
short *s_ln, *c_ln;        /* dummy arguments */
{
   return(csetup(system, in, out, srvc, s_ln, c_ln));
}
#else
int client_sock_setup(system, in, out, srvc)              /* UNIX ETC... */
char  *system;
int   *in, *out;
char  *srvc;
{
   return(csetup(system, in, out, srvc));
}
#endif /* APOFTN */
#endif /* VMS */
 
int sock_close(sock)
int *sock;
{
   return(sclose(sock));
}
 
#ifdef VMS
int sock_sendstr(sock, bufdes, m)      /* VMS CASE... */
int *sock;
struct dsc$descriptor_s  *bufdes;
int *m;
{
   return(ssendstr(sock, bufdes, m));
}
#else
int sock_sendstr(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssendstr(sock, buf, m));
}
#endif /* VMS */
 
#ifdef VMS
int sock_recvstr(sock, bufdes, m)      /* VMS CASE... */
int *sock;
struct dsc$descriptor_s  *bufdes;
int *m;
{
   return(srecvstr(sock, bufdes, m));
}
#else
int sock_recvstr(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecvstr(sock, buf, m));
}
#endif /* VMS */
 
int sock_send(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssend(sock, buf, m));
}
 
int sock_recv(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecv(sock, buf, m));
}
 
#ifndef apollo
/***  UNIX routines f77 <-> C to handle underlines generated by FORTRAN ***/
/***  CASE FOR SYSTEMS USING FULL-LENGTH NAMES (WITH UNDERLINES) IN F77 ***/
 
int client_sock_setup_(system, in, out, srvc)
char *system;
int  *in, *out;
char  *srvc;
{
   return(csetup(system, in, out, srvc));
}
 
#ifndef OSK
#ifndef VM
int inetd_sock_setup_(in, out)
int  *in, *out;
{
   return(isetup(in, out));
}
 
int inetd_sock_close_(sock)
int *sock;
{
   return(iclose(sock));
}
#endif /* ^VM */
#endif /* OSK */
 
int server_sock_setup_(in, out)
int  *in, *out;
{
   return(ssetup(in, out));
}
 
int sock_close_(sock)
int *sock;
{
   return(sclose(sock));
}
 
int sock_sendstr_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssendstr(sock, buf, m));
}
 
int sock_recvstr_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecvstr(sock, buf, m));
}
 
int sock_send_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssend(sock, buf, m));
}
 
int sock_recv_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecv(sock, buf, m));
}
 
/***  CASE FOR SYSTEMS USING SHORTENED NAMES (WITHOUT UNDERLINES) IN F77 ***/
/***   (NEEDED FOR SOME ULTRIX VERSIONS....)                             ***/
 
int csetup_(system, in, out, srvc)
char *system;
int  *in, *out;
char  *srvc;
{
   return(csetup(system, in, out, srvc));
}
#ifndef OSK
#ifndef VM
 
int isetup_(in, out)
int  *in, *out;
{
   return(isetup(in, out));
}
 
int iclose_(sock)
int *sock;
{
   return(iclose(sock));
}
#endif /* ^VM */
#endif /* OSK */
 
int ssetup_(in, out)
int  *in, *out;
{
   return(ssetup(in, out));
}
 
int sclose_(sock)
int *sock;
{
   return(sclose(sock));
}
 
int ssendstr_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssendstr(sock, buf, m));
}
 
int srecvstr_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecvstr(sock, buf, m));
}
 
int ssend_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssend(sock, buf, m));
}
 
int srecv_(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecv(sock, buf, m));
}
#endif /* apollo */
 
#ifdef CRAYFTN
/****   SPECIAL VERSION FOR CRAY CFT77 ***/
 
int CSETUP(system, in, out, srvc)
_fcd system;
int  *in, *out;
_fcd srvc;
{
   return(csetup(system, in, out, srvc));
}
 
int ISETUP(in, out)
int  *in, *out;
{
   return(isetup(in, out));
}
 
int ICLOSE(sock)
int *sock;
{
   return(iclose(sock));
}
 
int SSETUP(in, out)
int  *in, *out;
{
   return(ssetup(in, out));
}
 
int SCLOSE(sock)
int *sock;
{
   return(sclose(sock));
}
 
int SSENDSTR(sock, buf, m)
int  *sock;
_fcd buf;
int  *m;
{
   return(ssendstr(sock, buf, m));
}
 
int SRECVSTR(sock, buf, m)
int  *sock;
_fcd buf;
int  *m;
{
   return(srecvstr(sock, buf, m));
}
 
int SSEND(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(ssend(sock, buf, m));
}
 
int SRECV(sock, buf, m)
int  *sock;
char *buf;
int  *m;
{
   return(srecv(sock, buf, m));
}
#endif /* CRAYFTN */
 
#ifdef OSK
/****   SPECIAL VERSION FOR OS9 ONLY ***/
 
char cserv[80],chost[80];
 
int CLIENT_SOCK_SETUP(host,isock,osock,service)
char *host, *service;
int *isock;
int *osock;
{
   return(client_sock_setup(strfc(chost,host,80),isock,osock,
       strfc(cserv,service,80)));
}
 
int SERVER_SOCK_SETUP(isock,osock)
int *isock;
int *osock;
{
   return(server_sock_setup(isock,osock));
}
 
int SOCK_CLOSE(sock)
int *sock;
 
{
   return(sock_close(sock));
}
 
int SOCK_SENDSTR(sock, buf, m)
int  *sock;
char *buf;
int  *m;
 
{
   return(sock_sendstr(sock, buf, m));
}
 
int SOCK_RECVSTR(sock, buf, m)
int  *sock;
char *buf;
int  *m;
 
{
   return(sock_recvstr(sock, buf, m));
}
 
int SOCK_SEND(sock, buf, m)
int  *sock;
char *buf;
int  *m;
 
{
   return(sock_send(sock, buf, m));
}
 
int SOCK_RECV(sock, buf, m)
int  *sock;
char *buf;
int  *m;
 
{
   return(sock_recv(sock, buf, m));
}
#endif /* OSK */
 
#ifdef VM
int cinit(int dummy)
{
/*  fprintf(stderr,"Initializing C environment...\n");*/
    return(0);         /*to init the c environment*/
}
 
/* get length of a Fortran string given as argument */
int gtlnfstr(plistaddr,argposition)
void *plistaddr;        /* &first_arg */
int argposition;       /* n for nth argument */
{
    int **secplist;
    int *poffset;
    char *bzstr;
 
    poffset =(int *)plistaddr;
    poffset = poffset - 1;
    bzstr =(char *)(poffset -1);
    if (!strcmp(bzstr,"BZ")){
        secplist =(int **)((char *)plistaddr + *poffset);
        return( **(secplist + argposition - 1));
    }
    else{
        return(-1);   /* no secondary plist when no character*n arg */
    }
}
#endif /* VM */
 
 
#endif
