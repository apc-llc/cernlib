/*
 * $Id: serror.c,v 1.3 1998/08/25 12:44:52 mclareni Exp $
 *
 * $Log: serror.c,v $
 * Revision 1.3  1998/08/25 12:44:52  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.2  1997/10/07 16:28:16  mclareni
 * Fix a problem occurring due to a double typedef with Multinet v4 header
 * and Dec C ver 4 (G. Folger).
 *
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*CMZ :          21/02/95  15.02.38  by  Jamie Shiers*/
/*CMZ :          21/02/95  09.42.49  by  Jamie Shiers*/
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] =  "@(#)serror.c 1.24 2/21/95  CERN CN-PDP/CS F. Hemmer";
#endif /* not lint */
 
/* serror.c     Global error reporting routines*/
 
#include <stdio.h>      /* standard input/output                        */
#include <errno.h>      /* error numbers and codes                      */
 
#include <serrno.h>     /* special error numbers and codes              */
#include <log.h>        /* logger functions                             */
 
#if defined(vms)
 
#if defined(TGV) && (TGV == 1) && !defined(MULTINET)
#define MULTINET        1
#endif /* TGV && !MULTINET */
 
#if defined(TWG) && (TWG == 1)
/* We generate a compiler error, because currently unsupported*/
!!! Win/TCP is not supported in this version. Sorry !!!
extern int      uerrno;
extern int      vmserrno;
#endif /* TWG */
 
#if defined(MULTINET) && (MULTINET == 1)
/* fix a problem occuring due to a double typedef with multinet v4 header
 *  and Dec C ver 4;
 *  GF, 6-Oct 97
 */
#if !defined(_DECC_V4_SOURCE) && !defined(__DECC_VER)
#define _DECC_V4_SOURCE
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#undef _DECC_V4_SOURCE
#else
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#endif
 
#endif /* MULTINET */
 
#if defined(TCPWARE) && (TCPWARE == 1)
#include "tcpware_include:socket.h"
#include "tcpware_include:sockerr.h"
#endif /* TCPWARE */
 
#if defined(UCX) && (UCX == 1)
#include <socket.h>
#include <types.h>
#endif /* UCX */
 
#else /* ! vms */
#if !defined(CERNLIB_QMGLIBC)
extern int      sys_nerr;       /* number of system error messages      */
extern char *sys_errlist[];     /* system error list                    */
#endif
#endif /* vms */
 
#include <net.h>                /*     networking specifics             */
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define perror  DECC$PERROR
#define strerror DECC$STRERROR
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
int     serrno;
char    *sys_serrlist[SEMAXERR-SEBASEOFF+2]=
        {"Error 0",
         "Host not known",
         "Service unknown",
         "Not a remote file",
         "Timed out",
         "Unsupported FORTRAN format",
         "Unknown FORTRAN option",
         "Incompatible FORTRAN options",
         "File name too long",
         "Can't open configuration file",
         "Version ID mismatch",
         "User buffer too small",
         "Invalid reply number",
         "User message too long",
         "Entry not found",
  "Internal error",
  "Connection closed by remote end",
  "Message daemon unable to reply",
  "Message daemon system error",
  "Permission denied",
  "Can't find interface name",
  "SYSREQ remote communication error",
  "Can't open mapping database",
  "No user mapping",
  "Unable to set VMS user",
  "Host did not return error number",
  "Host not is local network and no mapping found",
         "BAD ERROR NUMBER"
        };
 
#if defined(vms)
#include <string.h>
#include <ssdef.h>
 
extern int sys$getmsg();
#define VMSMAXMSGLEN 256
static char vmsmsg[256];
struct {         /* VMS generic descriptor               */
        int     len;
        char    *buf;
} msgbuf = {
        VMSMAXMSGLEN, vmsmsg };
#endif /* vms */
 
#if defined(vax) && (vax == 1)
/*
 * The VAX C COMPILER doesn't define MAX_ERRNO_VALUE, and is therefore
 * incompatible with DEC C. Thank you DEC !. We use this horrible
 * kludge to search by hand the MAX_ERRNO_VALUE, and define it here.
 * A simplistic test is made in case errno's change.
 */
#if (EHOSTUNREACH != 65)
!! Incompatible version !!
#else
#if !defined(MAX_ERRNO_VALUE)
#define MAX_ERRNO_VALUE EHOSTUNREACH
#endif /* !MAX_ERRNO_VALUE */
#endif /* EHOSTUNREACH != 65 */
#endif /* vax */
 
#if defined(vms)
/*
 * DECC V4.0 has changed again ! MAX_ERRNO_VALUE is now
 * __ERRNO_MAX ! Looks like the QA guy in DEC moved
 * elsewhere !
 */
#if !defined(MAX_ERRNO_VALUE)
#if defined(__ERRNO_MAX)
#define MAX_ERRNO_VALUE __ERRNO_MAX
#endif /* __ERRNO_MAX */
#endif /* ! MAX_ERRNO_VALUE */
#endif /* vms */
 
static char buf[80];
 
char *  sstrerror(n)
int     n;
{
#if defined(vms)
        register int rc;
        int     msglen;
#endif /* vms */
 
        if ((n>SEBASEOFF) && (n<=SEMAXERR))   {
                return(sys_serrlist[n-SEBASEOFF]);
        }
        else {
#if !defined(vms)
                if ((n>0) && (n<sys_nerr)) {
                        return(sys_errlist[n]);
                }
                else {
                        (void) sprintf(buf,"%s: %d\n",
 sys_serrlist[SEMAXERR+1-SEBASEOFF],
                            n);
                        return(buf);
                }
#else /* vms */
/*
 * There are (were) some bugs is DEC C compilers (/OPT), just check
 * one of those.
 */
                if ((unsigned int)n <= (unsigned int)MAX_ERRNO_VALUE)
   log(LOG_ALERT,"%16.16X <= %16.16X\n", n, MAX_ERRNO_VALUE);
                if ((unsigned int)n > (unsigned int)MAX_ERRNO_VALUE)
   log(LOG_ALERT,"%16.16X > %16.16X\n", n, MAX_ERRNO_VALUE);
 
                if ((n > 0) && (n <= MAX_ERRNO_VALUE)) {
                        return(strerror(n));
                }
                else {
                        if (n == EVMSERR) {
                                /* We assume, error was last vms error */
                                /*FH bad assumption, as we're maybe in another
 process !*/
                                /* Later, we'll use subcodes */
#if defined(MULTINET) && (MULTINET == 1)
                                if (!vmserrno && !vaxc$errno) {
                                        sprintf(msgbuf.buf,
"%%system-f-vmserr, vms error code: 0x0: internal error");
                                        return(msgbuf.buf);
                                }
                                if (vmserrno)
                                        if ((rc = sys$getmsg(vmserrno,&msglen,
                                            &msgbuf,0,0)) != SS$_NORMAL) {
 
 sprintf(msgbuf.buf,
"%%system-f-vmserr, vms error code: 0x%x, unable to get vms message",
                                                    vmserrno);
                                                return(msgbuf.buf);
                                        }
                                        else {
                                                msgbuf.buf[msgbuf.len]='\0';
                                                return(msgbuf.buf);
                                        }
                                if (vaxc$errno)
                                        if ((rc = sys$getmsg(vaxc$errno,&
                                            msglen,&msgbuf,0,0)) != SS$_NORMAL)
 {
 
 sprintf(msgbuf.buf,
 "%%system-f-vmserr, vms error code: 0x%x, unable to get vms message",
                                                    vaxc$errno);
                                                return(msgbuf.buf);
                                        }
                                        else {
                                                msgbuf.buf[msgbuf.len]='\0';
                                                return(msgbuf.buf);
                                        }
#endif /* MULTINET */
#if ((defined(TCPWARE) && (TCPWARE == 1)) || (defined(UCX) && (UCX == 1)))
                                if (!vaxc$errno) {
                                        sprintf(msgbuf.buf,
"%%system-f-vmserr, vms error code: 0x0: internal error");
                                        return(msgbuf.buf);
                                }
                                if ((rc = sys$getmsg(vaxc$errno,&msglen,&
                                    msgbuf,0,0)) != SS$_NORMAL) {
                                        sprintf(msgbuf.buf,
"%%system-f-vmserr, vms error code: 0x%x, unable to get vms message",
                                            vaxc$errno);
                                        return(msgbuf.buf);
                                }
                                else {
                                        msgbuf.buf[msgbuf.len]='\0';
                                        return(msgbuf.buf);
                                }
#endif /* TCPWARE || UCX */
                        }
                        else {
                                (void) sprintf(buf,"%s: (%d) %d\n",
 sys_serrlist[SEMAXERR+1-SEBASEOFF],
                                    MAX_ERRNO_VALUE,n);
                                return(buf);
                        }
                }
#endif /* vms */
        }
}
 
void sperror(msg)
char    *msg;
{
        if (serrno)     {
                if ((serrno>SEBASEOFF) && (serrno<=SEMAXERR))   {
                        fprintf(stderr,"%s: %s\n",msg,sstrerror(serrno));
                }
                else    {
                        fprintf(stderr,"%s: %s: %d\n",msg,
                                sstrerror(SEMAXERR+1), serrno);
                }
        }
        else    {
#if !defined(vms)
                perror(msg);
#else   /* vms */
#if defined(TWG) && (TWG == 1)
  if (uerrno != 0)        {
   /* We force this error in errno, then perror */
   errno = uerrno;
   perror(msg);
   return;
  }
  if (vmserrno != 0)      {
   errno = EVMSERR;
   vaxc$errno = vmserrno;
  }
  perror(msg);
#endif /* TWG */
#if defined(MULTINET) && (MULTINET == 1)
  if (socket_errno != 0)        {
   socket_perror(msg);
   return;
  }
  if (vmserrno != 0)      {
   errno = EVMSERR;
   vaxc$errno = vmserrno;
  }
  perror(msg);
#endif /* MULTINET */
 
#if defined(TCPWARE) && (TCPWARE == 1)
                pneterror(msg);
                return;
#endif /* TCPWARE */
 
#if defined(UCX) && (UCX == 1)
                perror(msg);
#endif /* UCX */
 
#endif /* vms */
        }
}

