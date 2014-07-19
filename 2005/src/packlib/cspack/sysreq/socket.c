/*
 * $Id: socket.c,v 1.3 1998/08/25 12:44:55 mclareni Exp $
 *
 * $Log: socket.c,v $
 * Revision 1.3  1998/08/25 12:44:55  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.2  1997/10/07 16:28:19  mclareni
 * Fix a problem occurring due to a double typedef with Multinet v4 header
 * and Dec C ver 4 (G. Folger).
 *
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
#if !defined(lint)
static char sccsid[] =  "@(#)socket.c   1.19 7/29/94  CERN CN-PDP/CS F. Hemmer";
#endif /* lint */
 
/* socket.c     Generalized network interface                           */
 
/*
 * Some support should be added for Multinet and UCX as well
 */
 
#define DEBUG   0
/* Define DUMP to print buffers contents - heavy debug mode             */
/* #define DUMP    0    */
 
#if defined(vms)
 
#if defined(TGV) && (TGV == 1) && !defined(MULTINET)
#define MULTINET        1
#endif /* TGV && !MULTINET */
 
#if (!defined(TWG) && !defined(MULTINET) && !defined(TCPWARE) && !defined(UCX))
/* We generate a compiler error (#error not defined everywhere!) */
!!! YOU MUST SPECIFY either MULTINET or TWG or TCPWARE or UCX !!!
#endif /* !MULTINET && !TWG && !TCPWARE && !UCX */
 
#if defined(MULTINET) && (MULTINET == 1)
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
#endif /* TCPWARE */
 
#if defined(UCX) && (UCX == 1)
#include <types.h>
#include <socket.h>
#endif /* UCX */
 
#endif /* vms */
 
#include <net.h>                        /* networking specifics         */
#include <log.h>                        /* logging functions            */
#include <serrno.h>                     /* special errors               */
 
#if defined(vms)
 
#if defined(TWG) && (TWG == 1)
#define READ(x,y,z)     netread(x,y,z)  /* VMS TWG socket read          */
#define WRITE(x,y,z)    netwrite(x,y,z) /* VMS TWG socket write         */
#endif /* TWG */
#if defined(MULTINET) && (MULTINET == 1)
#define READ(x,y,z)     socket_read(x,y,z)  /* VMS MULTINET socket read */
#define WRITE(x,y,z)    socket_write(x,y,z) /* VMS MULTINET socket write*/
#define CLOSE(x)        socket_close(x)     /* VMS MULTINET socket close*/
#define IOCTL(x,y,z)    socket_ioctl(x,y,z) /* VMS MULTINET socket ioctl*/
#endif /* MULTINET */
#if defined(TCPWARE) && (TCPWARE == 1)
#define READ(x,y,z)     socket_read(x,y,z)  /* VMS TCPWARE socket read   */
#define WRITE(x,y,z)    socket_write(x,y,z) /* VMS TCPWARE socket write  */
#define CLOSE(x)        socket_close(x)     /* VMS TCPWARE socket close  */
#define IOCTL(x,y,z)    socket_ioctl(x,y,z) /* VMS TCPWARE socket ioctl  */
#endif /* TCPWARE */
#if defined(UCX) && (UCX==1)
#define READ(x,y,z)     read(x,y,z)     /* Actual read system call      */
#define WRITE(x,y,z)    write(x,y,z)    /* Actual write system  call    */
#define CLOSE(x)        close(x)        /* Actual close system call     */
#define IOCTL(x,y,z)    ioctl(x,y,z)    /* Actual ioctl system call     */
#endif /* UCX */
 
#include <string.h>
#include <ssdef.h>
 
extern int sys$getmsg();
#define VMSMAXMSGLEN 256
 
static char vmsmsg[VMSMAXMSGLEN];
static struct {         /* VMS generic descriptor               */
        int     len;
        char    *buf;
} msgbuf = {
        VMSMAXMSGLEN, vmsmsg };
 
#else /* ! vms */
 
#define READ(x,y,z)     recv(x,y,z,0)   /* Actual read system call      */
#define WRITE(x,y,z)    send(x,y,z,0)   /* Actual write system  call    */
#define CLOSE(x)        close(x)        /* Actual close system call     */
#define IOCTL(x,y,z)    ioctl(x,y,z)    /* Actual ioctl system call     */
 
#if !defined(CERNLIB_QMGLIBC)
extern int      sys_nerr;       /* number of system error messages      */
extern char *sys_errlist[];     /* system error list                    */
#endif
#endif /* vms */
 
#ifndef READTIMEOUTVALUE
#define READTIMEOUTVALUE     60         /* Default read time out        */
#endif /* READTIMEOUTVALUE */
 
static int      rtimeout=READTIMEOUTVALUE;
 
/*
 * Define BLOCKSIZE if read/write calls have a length upper limit
 * E.g. VMS QIO calls are limited to transfer 65536 bytes.
 */
 
#if defined(vms)
#define BLOCKSIZE       32768
#else
#ifdef BLOCKSIZE
#undef BLOCKSIZE        /* be safe      */
#endif /* BLOCKSIZE */
#endif /* vms */
 
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
extern char* malloc();
 
#if defined(vms)
#if defined(TWG) && (TWG == 1)
extern int      uerrno;
extern int      vmserrno;
#define errno   (uerrno ? uerrno : vmserrno)
#endif /* TWG */
#if defined(MULTINET) && (MULTINET == 1)
extern volatile int noshare      socket_errno;
extern volatile int noshare      vmserrno;
#define errno   (socket_errno ? socket_errno : vmserrno)
#endif /* MULTINET */
#endif /* vms */
 
static jmp_buf alarmbuf;
static void     (* defsigalrm) ();
 
#ifndef min
#define min(x, y)       (((x-y) > 0) ? y : x)
#endif /* min */
 
/*
 * Statistics
 */
 
struct iostats  {
        unsigned int    rcount;         /* read() count                 */
        unsigned int    wcount;         /* write() count                */
        unsigned int    rbcount;        /* byte(s) read                 */
        unsigned int    wbcount;        /* byte(s) written              */
} iostats = { 0,0,0,0 };
 
 
/*
 * This is needed for CRAY/Ultranet bug, crashing if write() buffer
 * is not word aligned.
 */
 
#ifdef CRAY
/* #define CHECK_ALIGN     1    */      /* Don't check for time being   */
#if defined (cray-xmp)
#define NOTALIGN(p)     ((int) ((((int)(p)) >> 61) & 0x00000007))
#endif /* cray-xmp */
#endif /* CRAY */
 
#ifdef DUMP
 
#include <ctype.h>
 
static  int
Dump(buf, nbytes)
char    *buf;
int     nbytes;
{
        register int    i, j;
        register char   c;
 
        log(LOG_DEBUG ," *** Hexadecimal dump *** \n");
 
        for (i=0;i<nbytes/20;i++)       {
                for (j=0;j<20;j++)      {
                        log(LOG_DEBUG ,"<%2.2X>", (char) buf[i*20+j]);
                }
                log(LOG_DEBUG ,"\n");
        }
        for (i=0;i<nbytes%20;i++)       {
                log(LOG_DEBUG ,"<%2.2X>",buf[(nbytes/20)*20+i]);
        }
        log(LOG_DEBUG ,"\n *** Interpreted dump *** \n");
        for (i=0;i<nbytes/80;i++)       {
                for (j=0;j<80;j++)      {
                        c = (char) buf[i*80+j];
                        if (isprint(c)) log(LOG_DEBUG ,"%c", c);
                        else log(LOG_DEBUG ,".");
                }
                log(LOG_DEBUG ,"\n");
        }
        for (i=0;i<nbytes%80;i++)       {
                c = (char) buf[(nbytes/80)*80+i];
                if (isprint(c)) log(LOG_DEBUG ,"%c", c);
                else log(LOG_DEBUG ,".");
        }
        log(LOG_DEBUG ,"\n");
}
#endif /* DUMP */
 
int     setrtimo(val)
int     val;
{
        register int    otimeout;
 
        otimeout = rtimeout;
        rtimeout=val;
        return(otimeout);
}
 
#ifdef READTIMEOUT
void    catch()
{
        longjmp(alarmbuf, 1);
}
#endif /* READTIMEOUT */
 
int
s_recv (s, buf, nbytes)
int     s;
char    *buf;
int     nbytes;
{
        register int    n, nb;
 
#if DEBUG
        log(LOG_DEBUG ,"dorecv(%x, %x, %d)\n", s, buf, nbytes);
#endif /* DEBUG */
 
#ifdef READTIMEOUT
        if (setjmp(alarmbuf) == 1)      {
                signal(SIGALRM, defsigalrm);    /* restore alarm handler*/
#if !defined(vms)
                errno = ETIMEDOUT;
#else /* !vms */
#if defined(TWG)
                uerrno = ETIMEDOUT;
#endif /* TWG */
#if defined(MULTINET)
                socket_errno = ETIMEDOUT;
#endif /* MULTINET */
#endif /* vms */
                return(-1);
        }
 
#if !defined(apollo)
        defsigalrm = signal (SIGALRM, (void (*)()) catch);
#else
/* DomainOS generates a warning :
 *      Conversion of "catch" to pointer of
 *      stricter alignment can cause serious performance degradation.
 */
        defsigalrm = signal (SIGALRM, catch);
#endif /* !apollo */
#endif /* READTIMEOUT */
 
        nb = nbytes;
        for (; nb >0;)       {
#ifdef CHECK_ALIGN
                if (NOTALIGN(buf))      {
#ifdef READTIMEOUT
                        alarm(0);
                        signal(SIGALRM, defsigalrm);
#endif /* READTIMEOUT */
                        log(LOG_DEBUG ,
                            "CRITICAL: unaligned buffer at %x\n",buf);
                        log(LOG_DEBUG ,
                            "CRITICAL: while dorecv(),n=%d,nb=%d,nbytes=%d\n",
                                        n,nb,nbytes);
                        errno = EFAULT;
                        return(-1);
                }
#endif /* CHECK_ALIGN */
#ifdef READTIMEOUT
                alarm(rtimeout);/* successive calls reset the alarm     */
#endif /* READTIMEOUT */
#ifdef BLOCKSIZE
                n = READ(s, buf, min(BLOCKSIZE, nb));
#else
                n = READ(s, buf, nb);
#endif
                nb -= n;
 
#ifdef READTIMEOUT
                alarm(0);
                signal(SIGALRM, defsigalrm);
#endif /* READTIMEOUT */
                if (n <= 0)      {
                        if (n == 0) {
                                serrno=SECONNDROP;
                                return(0);
                        }
#if DEBUG
                        log(LOG_DEBUG ,
                                "ERROR: %d while n=%d,nb-n=%d,buf=%x\n",
                                errno, n, nb, buf);
#endif /* DEBUG */
                        return (n);
                }
#if DEBUG
                log(LOG_DEBUG ,"dorecv: %d bytes received\n",n);
#if DUMP
                log(LOG_DEBUG ,"dorecv: dump follows\n");
                Dump(buf,n);
#endif /* DUMP */
#endif /* DEBUG */
                iostats.rcount++;
                buf += n;
        }
        iostats.rbcount += nbytes;
        return (nbytes);
}
 
int
s_send (s, buf, nbytes)
int     s;
char    *buf;
int     nbytes;
{
        register int    n, nb;
 
#if DEBUG
        log(LOG_DEBUG, "dosend(%x, %x, %d)\n", s, buf, nbytes);
#endif
        nb = nbytes;
        for (; nb >0;)       {
#ifdef  CHECK_ALIGN
                if (NOTALIGN(buf))      {
                        log(LOG_DEBUG ,"CRITICAL: unaligned buffer at %x\n",
                                                buf);
                        log(LOG_DEBUG ,
                           "CRITICAL: while dosend(),n=%d,nb=%d,nbytes=%d\n",
                                n,nb,nbytes);
                        errno = EFAULT;
                        return(-1);
                }
#endif /* CHECK_ALIGN */
#ifdef BLOCKSIZE
                n = WRITE(s, buf, min(BLOCKSIZE, nb));
#else
                n = WRITE(s, buf, nb);
#endif
                nb -= n;
                if (n <= 0)      {
                        if (n == 0) {
                                serrno=SECONNDROP;
                                return(0);
                        }
#if DEBUG
                        log(LOG_DEBUG ,
                                "ERROR: %d while n=%d,nb-n=%d,buf=%x\n",
                                errno, n, nb, buf);
#endif /* DEBUG */
                        return (n);
                }
#if DEBUG
#if DUMP
                log(LOG_DEBUG ,"dosend: dump follows\n");
                Dump(buf,n);
#endif /* DUMP */
                log(LOG_DEBUG ,"dosend: %d bytes sent\n",n);
#endif /* DEBUG */
                iostats.wcount++;
                buf += n;
        }
        iostats.wbcount+=nbytes;
        log(LOG_DEBUG, "dosend(%x) returns %d\n", s, nbytes);
        return (nbytes);
}
 
int
s_close(s)
int     s;
{
        return(CLOSE(s));
}
 
char *
s_errmsg()                              /* return last error message    */
{
#if !defined(vms)
        return(sys_errlist[errno]);
#else /* vms */
#if defined(MULTINET) && (MULTINET == 1)
        return(vms_errno_string());
#endif /* MULTINET */
#if defined(TCPWARE) && (TCPWARE == 1)
        if ((rc = sys$getmsg(vaxc$errno,&msglen,&msgbuf,0,0)) != SS$_NORMAL) {
                sprintf(msgbuf.buf,"%%system-f-vmserr, vms error code: 0x%x, \
unable to get vms message", vaxc$errno);
                return(msgbuf.buf);
        }
        else {
                msgbuf.buf[msgbuf.len]='\0';
                return(msgbuf.buf);
        }
#endif /* TCPWARE */
#if defined(UCX) && (UCX == 1)
#pragma nostandard
        return(strerror(errno, vaxc$errno));
#pragma standard
#endif /* UCX */
#endif /* vms */
}
 
/*
 * Solaris 2.x defines but does not document a s_ioctl routine
 * in libsocket.a, therefore conflicting with ours. Hence this
 * workaround.
 */
 
#if defined(SOLARIS) && (SOLARIS == 1)
#define s_ioctl sol_s_ioctl
#endif /* SOLARIS */
 
int
s_ioctl(s, request, arg)                /* issue an ioctl(2) call       */
int s, request;
int     arg;
{
        return(IOCTL(s, request, arg));
}
 
int s_nrecv(s, buf, nbytes)     /* Non blocking read                    */
int     s, nbytes;
char    *buf;
{
#if defined(BLOCKSIZE)
        return(READ(s, buf, min(BLOCKSIZE,nbytes)));
#else
        return(READ(s, buf, nbytes));
#endif /* BLOCKSIZE */
}
 
void    getiostat(iostatbuf)            /* return raw I/O statitics     */
struct iostats  *iostatbuf;
{
        iostatbuf->rcount = iostats.rcount;
        iostatbuf->wcount = iostats.wcount;
        iostatbuf->rbcount = iostats.rbcount;
        iostatbuf->wbcount = iostats.wbcount;
}
 
int (*recvfunc)()=s_recv;               /* recv function to use         */
int (*sendfunc)()=s_send;               /* send function to use         */
int (*closefunc)()=s_close;             /* close function to use        */
int (*ioctlfunc)()=s_ioctl;             /* ioctl function to use        */
char *(*errfunc)()=s_errmsg;           /* strerror function to use     */

