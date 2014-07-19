/*
 * $Id: netreq.c,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
 *
 * $Log: netreq.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] = "@(#)netreq.c	1.18 10/14/94 CERN CN-PDP/CS F. Hemmer";
#endif /* not lint */
 
/* netreq.c     Network interface to the SYSREQ communication system    */
 
#include "config.h"     /* configuration parameters                     */
#if !defined(vms)
#include <sysreq.h>     /* SYSREQ specific definitions                  */
#include <sys/types.h>  /* Standard data types                          */
#include <sys/socket.h> /* Socket interface                             */
#include <netinet/in.h> /* Internet data types                          */
#include <arpa/inet.h>  /* Arpa internet routines                       */
#include <stdio.h>      /* Standard Input/Output                        */
#if defined(vms)
#include <pwd_vms.h>
#else
#include <pwd.h>
#endif
#include <netdb.h>      /* Network "data base"                          */
#include <errno.h>      /* Error numbers                                */
#include <serrno.h>     /* Special Error numbers                        */
#include <trace.h>      /* tracing definitions                          */
#endif /* vms */
#if defined(vms)
#include "sysreq.h"     /* SYSREQ specific definitions                  */
#include <types.h>      /* Standard data types                          */
#include <socket.h>     /* Socket interface                             */
#include <in.h>         /* Internet data types                          */
#include <inet.h>       /* Arpa internet routines                       */
#include <stdio.h>      /* Standard Input/Output                        */
#include <netdb.h>      /* Network "data base"                          */
#include <errno.h>      /* Error numbers                                */
#include "serrno.h"     /* Special Error numbers                        */
#include "trace.h"      /* tracing definitions                          */
 
#if defined(TGV) && (TGV == 1) && !defined(MULTINET)
#define MULTINET        1
#endif /* TGV && !MULTINET */
 
#if (!defined(TWG) && !defined(MULTINET) && !defined(TCPWARE) && !defined(UCX))
/* We generate a compiler error (#error not defined everywhere!) */
!!! YOU MUST SPECIFY either MULTINET or TWG or TCPWARE or UCX !!!
 
#endif /* !MULTINET && !TWG && !TCPWARE && !UCX */
#ifdef TWG
#define close(x)  netclose(x)   /* TWG network close                    */
extern int      uerrno;
extern int      vmserrno;
#define perror(x) fprintf(stderr,"%s: TWG error %d\n",x,uerrno);
#define errno   (uerrno ? uerrno : vmserrno)
#endif /* TWG */
#if defined(MULTINET) && (MULTINET == 1)
#define close(x)  socket_close(x)   /* MULTINET network close           */
#define perror(x) socket_perror(x)  /* MULTINET network error           */
extern volatile int noshare      socket_errno;
extern volatile int noshare      vmserrno;
#define errno   (socket_errno ? socket_errno : vmserrno)
#endif /* MULTINET */
#include <ssdef.h>      /* system services retcodes definitions         */
#include <jpidef.h>     /* job and process information                  */
#include <prvdef.h>     /* privileges definitions                       */
 
#endif /* vms */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define atoi    DECC$ATOI
#define free    DECC$FREE
#define getenv  DECC$GETENV
#define strcpy  DECC$STRCPY
#define strcmp  DECC$STRCMP
#define strlen  DECC$STRLEN
#define strncpy DECC$STRNCPY
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
extern  char    *getconfent();
 
#ifdef vms
#define USERNAME_L      12
typedef struct  {
	short           bufl;
	short           code;
	char            *buf;
	unsigned int    *retl;
} Item;
 
char    username[USERNAME_L+1];         /* username buffer              */
unsigned int    usernamel;              /* length of dito               */
unsigned int    curpriv[2];             /* current process privileges   */
unsigned int    curprivl;               /* length of dito               */
 
static struct   {
	Item    item[2];
	int     eol;
} itemlist = {
	{
	{ sizeof(curpriv),      JPI$_CURPRIV,   curpriv,        &curprivl },
	{ sizeof(username),     JPI$_USERNAME,  username,       &usernamel},
	},
	0
};
 
#endif /* vms */
 
int
_sysreq(loginid, acct, sernam, comtxt, comtxtl, reptxt, arysz)
char    *loginid;       /* requestor's login id                         */
char    *acct;          /* requestor's account                          */
char    *sernam;        /* Service name                                 */
char    *comtxt;        /* Command text                                 */
short   comtxtl;        /* Command text length                          */
char    *reptxt;        /* Reply text                                   */
int     *arysz;         /* Reply array size, actual size on return      */
{
        char    sysreq_host[100];
	short   sysreq_port;    /* SYSREQ service port number           */
        struct servent  *sp;    /* services pointer                     */
        struct hostent  *hp;    /* host pointer                         */
        struct sockaddr_in sin; /* internet socket                      */
        int     s;              /* socket descriptor                    */
	LONG magic;             /* to hold magic number trust           */
        WORD    reqtype;        /* to hold the request type             */
        char    *p;             /* a character array pointer            */
	int     replylen;       /* SYSREQ reply length                  */
        int     sysreqrc;       /* SYSREQ return code                   */
#ifdef unix
	struct  passwd  *pw;    /* Password entry structure             */
#endif /* unix */
#ifdef vms
	int     rc;             /* system services return code          */
#endif /* vms */
 
        sysreq_port = SYSREQ_PORT;
 
/*
 * Get some run-time options
 */
 
	INIT_TRACE("SYSREQ_TRACE");
        if ((p = getenv("SYSREQ_HOST")) != NULL)       {
                strcpy(sysreq_host, p);
        }
        else    {
                if ((p = getconfent("SYSREQ", "HOST",0)) == NULL ||
                    strcmp(p,"") == 0) {
                        strcpy (sysreq_host,SYSREQ_HOST);
                }
                else    {
                        strcpy(sysreq_host, p);
                }
        }
        if ((p = getenv("SYSREQ_PORT")) != NULL)       {
                sysreq_port = atoi(p);
        }
 
	TRACE(1, "sysreq", "_sysreq(%s,%s,%s,%d,%x,%d) entered",
		 loginid, sernam, comtxt, comtxtl, reptxt, *arysz);
 
        sin.sin_family = AF_INET;
 
	TRACE(2, "sysreq", "Looking for host %s",sysreq_host);
        hp = gethostbyname(sysreq_host);
        if (hp == NULL) {
                serrno = SENOSHOST;
		END_TRACE();
                return(-SENOSHOST);
        }
        sin.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
 
#if SERVICESDB
        if ((p = getenv("SYSREQ_PORT")) == NULL)       {
		TRACE(2, "sysreq", "Looking for service %s on %s",
			 SYSREQ_NAME, SYSREQ_PROTO);
                if ((sp = getservbyname(SYSREQ_NAME, SYSREQ_PROTO)) == NULL) {
                        serrno = SENOSSERV;
			END_TRACE();
                        return(-SENOSSERV);
                }
                sin.sin_port = sp->s_port;
                sysreq_port = ntohs(sp->s_port);
        }
#endif /* services */
	TRACE(2, "sysreq", "Assigning port %d to %s using %s",
		 sysreq_port, SYSREQ_NAME, SYSREQ_PROTO);
        sin.sin_port = htons(sysreq_port);
 
	TRACE(2, "sysreq", "creating socket");
        if ((s = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		TRACE(2, "sysreq", "socket(): (errno=%d)", errno);
		END_TRACE();
                return(-errno);
        }
 
	TRACE(2, "sysreq", "connecting %s port %d", sysreq_host, sysreq_port);
        if (connect(s, (struct sockaddr *)&sin, sizeof(struct sockaddr_in)) ==
 -1) {
		TRACE(2, "sysreq", "connect(): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
 
        magic = C_MAGIC;
	TRACE(2, "sysreq", "send magic number: %x",C_MAGIC);
        if (SendLong(s, &magic) < 0)    {
		TRACE(2, "sysreq", "SendLong(magic): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "receive magic number");
        if (RecvLong(s, &magic) < 0)    {
		TRACE(2, "sysreq", "RecvLong(magic): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "server magic: %x", magic);
        if (magic != S_MAGIC)    {
		TRACE(2, "sysreq", "server: bad magic number: %x", magic);
                (void) close(s);
                serrno = SEBADVERSION;
		END_TRACE();
                return(-SEBADVERSION);
        }
        if (loginid == (char *)0) loginid = "";
/*
 * send out effective id (i.e. the one really issuing the request
 */
#ifdef unix
        pw = getpwuid(geteuid());
        if (strcmp(pw->pw_name,loginid)) {
                if (strcmp(pw->pw_name,"root"))    {    /* Not authorized */
			TRACE(2, "sysreq", "user %s not authorized",
					pw->pw_name);
                        loginid = pw->pw_name;
                }
        }
	TRACE(2, "sysreq","send loginid: %s",loginid);
        if ((int) SendStr(s, loginid, strlen(loginid)) < 0)     {
		TRACE(2, "sysreq", "SendStr(loginid): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq","send real user: %s", pw->pw_name);
        if ((int) SendStr(s, pw->pw_name, strlen(pw->pw_name)) < 0)     {
		TRACE(2, "sysreq", "SendStr(realuser): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
#endif /* unix */
#ifdef vms
	TRACE(2, "sysreq","send loginid: %s",loginid);
	/* if PRV$_SYSPRV is granted, we issue the command as SYSTEM */
	if ((rc = sys$getjpi(0, 0, 0, &itemlist, 0, 0, 0)) != SS$_NORMAL) {
                if (!(rc & 0x01))       {
                        vaxc$errno = rc;
#if defined(MULTINET) && (MULTINET == 1)
			vmserrno = EVMSERR;
#endif /* MULTINET */
#if defined(TWG) && (TWG == 1)
			vmserrno = EVMSERR;
#endif /* TWG */
#if defined(UCX) && (UCX == 1)
			errno = EVMSERR;
#endif /* UCX */
			return(-errno);
                }
	}
	username[usernamel]='\0';
	if (curpriv[0] & PRV$M_SYSPRV)  { /* pretend being SYSTEM */
		loginid = "SYSTEM";
	}
 
        if ((int) SendStr(s, loginid, strlen(loginid)) < 0)     {
		TRACE(2, "sysreq", "SendStr(loginid): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq","send real user: %s", username);
        if ((int) SendStr(s, username, strlen(username)) < 0)     {
		TRACE(2, "sysreq", "SendStr(realuser): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
#endif /* vms */
	TRACE(2, "sysreq", "send account: %s",acct);
        if ((int) SendStr(s, acct, strlen(acct)) < 0)     {
		TRACE(2, "sysreq", "SendStr(account): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
 
/*
 * Process here
 */
        reqtype = SYSREQTYPE;
	TRACE(2, "sysreq", "send request type: 0X%X", reqtype);
        if ((int) SendWord(s, &reqtype) < 0)     {
		TRACE(2, "sysreq", "SendWord(ReqType): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
 
	TRACE(2, "sysreq", "send service name: %s",sernam);
        if ((int) SendStr(s, sernam, strlen(sernam)) < 0)     {
		TRACE(2, "sysreq", "SendStr(ServiceName): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "send comm. text: <%s>",comtxt);
        if ((int) SendStr(s, comtxt, comtxtl) < 0)     {
		TRACE(2, "sysreq", "SendStr(CommTxt): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "recv reply length");
        if ((int)(p = RecvStr(s, &replylen)) <0)     {
		TRACE(2, "sysreq", "RecvStr(RepLen): (errno=%d)", errno);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "reply length is %d byte(s) long", replylen);
	TRACE(2, "sysreq", "recv return code");
        if (RecvLong(s, &sysreqrc) <0)     {
		TRACE(2, "sysreq", "RecvLong(RetCode): (errno=%d)", errno);
		(void) free(p);
                (void) close(s);
		END_TRACE();
                return(-errno);
        }
	TRACE(2, "sysreq", "return code is %d (dec)", sysreqrc);
 
	TRACE(2, "sysreq", "user buffer at %x is %d byte(s) long",
			reptxt, *arysz);
        if (*arysz-1 < replylen)  {
		TRACE(2, "sysreq", "strncpy(%x,%x,%d)", reptxt,p,*arysz-1);
                strncpy(reptxt, p, *arysz-1);
                reptxt[*arysz] = '\0';
                free(p);
		TRACE(2, "sysreq", "reply buffer too short");
		TRACE(2, "sysreq", "shutdown socket");
                (void) shutdown(s, 2);
                (void) close(s);
                serrno = SEUBUF2SMALL;
		END_TRACE();
                return(-SEUBUF2SMALL);
        }
	TRACE(2, "sysreq", "strncpy(%x,%x,%d)", reptxt,p,replylen);
        strncpy(reptxt, p, replylen);
        *arysz = replylen;
        free(p);
	TRACE(2, "sysreq", "shutdown socket");
        (void) shutdown(s, 2);
        (void) close(s);
	END_TRACE();
        return(sysreqrc);
}
