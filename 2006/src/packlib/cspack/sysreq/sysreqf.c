/*
 * $Id: sysreqf.c,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
 *
 * $Log: sysreqf.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] =
"@(#)sysreqf.c  1.18 7/14/94 CERN CN-SW/DC Frederic Hemmer";
#endif /* not lint */
 
/* sysreqf.c    Fortran to C SYSREQ interface                           */
 
/* This should really go in a osdep/config file                         */
#if defined(sun)
#define UNIXF77 1
#endif /* sun */
#if defined(sgi)
#define UNIXF77 1
#endif /* sgi */
#if (defined(mips) && defined(ultrix))
#define UNIXF77 1
#endif /* mips && ultrix */
#if (defined(apollo) && defined(F77))
#define UNIXF77 1
#endif /* apollo && F77 */
#if defined(_AIX) && defined(_IBMESA)
#define UNIXF77 1
#endif /* AIX && IBMESA */
#if defined(_AIX) && defined(_IBMR2)
#define UNIXF77 1
#endif /* AIX && IBMR2 */
#if defined(hpux)
#define UNIXF77 1
#endif /* hpux */
#if defined(__osf__) && defined(__alpha)
#define UNIXF77 1
#endif /* __osf__ && __alpha */
 
#include <stdio.h>                      /* Standard Input/Output        */
#include <errno.h>                      /* Error numbers and codes      */
#ifndef vms
#include <sysreq.h>                     /* Sysreq interface definitions */
#include <trace.h>                      /* tracing definitions          */
#else
#include "sysreq.h"                     /* SYSREQ specific definitions  */
#include "trace.h"                      /* tracing definitions          */
#endif /* vms */
#ifdef unix
#if defined(vms)
#include <pwd_vms.h>
#else
#include <pwd.h>
#endif
#endif /* unix */
 
#if defined(CRAY)
#include <fortran.h>                    /* Fortran/C CRAY interface     */
#endif /* CRAY */
#if defined(vms)
#include <descrip.h>                    /* Argument Descriptor formats  */
#endif /* vms */
 
static int      debug=0;
 
static int sysreq_tracing = 0;
extern int _sysreq();
 
#if defined(CRAY)
void SYSREQ(fsernam, fcomtxt, rcode,freptxt,farysz)
_fcd    fsernam, fcomtxt, freptxt;
int     *rcode,*farysz;
{
        char    *sernam, *comtxt, *reptxt;
        char    *fcp, *p;
        int     len, arylen, arysz;
        register int    i,j,k;
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "SYSREQ(%x,%x,%x,%x,%d) entered",
                  fsernam, fcomtxt, rcode, freptxt, arysz);
 
/*
 * Converting fortran arrays
 */
 
        fcp = _fcdtocp(fsernam);
        len = _fcdlen(fsernam);
        if ((sernam = malloc(len+1)) == NULL)   {
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(sernam, fcp, len); sernam[len] = '\0';
        TRACE(2, "sysreq", "FORTRAN service name: %s",sernam);
 
        fcp = _fcdtocp(fcomtxt);
        len = _fcdlen(fcomtxt);
        if ((comtxt = malloc(len+1)) == NULL)   {
                free(sernam);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(comtxt, fcp, len); comtxt[len] = '\0';
        TRACE(2, "sysreq", "FORTRAN comm. text: %s",comtxt);
 
        fcp = _fcdtocp(freptxt);
        arylen = _fcdlen(freptxt);
        arysz = *farysz*arylen;
/*
 * Blank fill the array
 */
        for(i=0;i<arysz;i++)      {
                fcp[i] = ' ';
        }
        if ((reptxt = malloc(arysz+1)) == NULL)   {
                (void) free(sernam);
                (void) free(comtxt);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        len = strlen(comtxt);
        *rcode = sysreq(sernam, comtxt, &len, reptxt, &arysz);
        (void) free(sernam);
        (void) free(comtxt);
/*
 * Copy back, skipping new lines.
 */
 
        for (i=0,j=0,k=1;(i<arysz)&&(k<=arylen);i++)     {
                if (reptxt[i]!= '\n')    {
                        fcp[j++] = reptxt[i];
                }
                else {
                        j = arylen*(k++);
                }
        }
        *farysz = k;
        (void) free(reptxt);
}
#endif /* CRAY */
 
#if (defined(apollo) && !defined(F77))
 
static int
do_sysreq(sernam, comtxt, comtxtl, reptxt, arysz)
char    *sernam;        /* Service name                                 */
char    *comtxt;        /* Command text                                 */
int     *comtxtl;       /* Command text length                          */
char    *reptxt;        /* Reply text                                   */
int     *arysz;         /* Reply array size, actual size on return      */
{
        char    *p;
#ifdef unix
        struct  passwd  *pw;
#endif /* unix */
        char    *acct;
        int     aid;
 
        acct = "        ";              /* This is the default value    */
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "do_sysreq(%s,%s,%d,%x,%d) entered",
                  sernam, comtxt, *comtxtl, reptxt, *arysz);
        pw = getpwuid(geteuid());
        END_TRACE();
        return(_sysreq(pw->pw_name, acct,sernam,
                        comtxt, *comtxtl, reptxt, arysz));
}
 
void SYSREQ(fsernam, fcomtxt, rcode,freptxt,farysz,
            fsernaml, fcomtxtl, freptxtl)
char    *fsernam, *fcomtxt, *freptxt;
short   *fsernaml, *fcomtxtl, *freptxtl;
int     *rcode,*farysz;
{
        char    *sernam, *comtxt, *reptxt;
        char    *fcp, *p;
        int     len, arylen, arysz;
        register int    i,j,k;
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "SYSREQ(%x,%x,%x,%x,%d) entered",
                  fsernam, fcomtxt, rcode, freptxt, arysz);
 
/*
 * Converting fortran arrays
 */
 
        if ((sernam = malloc(*fsernaml+1)) == NULL)   {
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(sernam, fsernam, *fsernaml); sernam[*fsernaml] = '\0';
        TRACE(2, "sysreq", "FORTRAN service name: %s",sernam);
 
        if ((comtxt = malloc(*fcomtxtl+1)) == NULL)   {
                free(sernam);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(comtxt, fcomtxt, *fcomtxtl); comtxt[*fcomtxtl] = '\0';
        TRACE(2, "sysreq", "FORTRAN comm. text: %s",comtxt);
 
        arylen = *freptxtl;
        arysz = *farysz*arylen;
/*
 * Blank fill the array
 */
        for(i=0;i<arysz;i++)      {
                freptxt[i] = ' ';
        }
        if ((reptxt = malloc(arysz+1)) == NULL)   {
                (void) free(sernam);
                (void) free(comtxt);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        len = strlen(comtxt);
        *rcode = do_sysreq(sernam, comtxt, &len, reptxt, &arysz);
        (void) free(sernam);
        (void) free(comtxt);
/*
 * Copy back, skipping new lines.
 */
 
        for (i=0,j=0,k=1;(i<arysz)&&(k<=arylen);i++)     {
                if (reptxt[i]!= '\n')    {
                        freptxt[j++] = reptxt[i];
                }
                else {
                        j = arylen*(k++);
                }
        }
        *farysz = k;
        (void) free(reptxt);
}
#endif /* apollo */
 
#if defined(UNIXF77)
 
void sysreq_(fsernam, fcomtxt, rcode,freptxt,farysz,
             fsernaml, fcomtxtl, freptxtl)
char    *fsernam, *fcomtxt, *freptxt;
int     fsernaml, fcomtxtl, freptxtl;
int     *rcode,*farysz;
{
        char    *sernam, *comtxt, *reptxt;
        char    *fcp, *p;
        int     len, arylen, arysz;
        register int    i,j,k;
 
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "SYSREQ(%x,%x,%x,%x,%d) entered",
                  fsernam, fcomtxt, rcode, freptxt, *farysz);
 
/*
 * Converting fortran arrays
 */
 
        if ((sernam = malloc(fsernaml+1)) == NULL)   {
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(sernam, fsernam, fsernaml); sernam[fsernaml] = '\0';
        TRACE(2, "sysreq", "FORTRAN service name: %s",sernam);
 
        if ((comtxt = malloc(fcomtxtl+1)) == NULL)   {
                free(sernam);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(comtxt, fcomtxt, fcomtxtl); comtxt[fcomtxtl] = '\0';
        TRACE(2, "sysreq", "FORTRAN comm. text: %s",comtxt);
 
        arylen = freptxtl;
        arysz = *farysz*arylen;
/*
 * Blank fill the array
 */
        for(i=0;i<arysz;i++)      {
                freptxt[i] = ' ';
        }
        if ((reptxt = malloc(arysz+1)) == NULL)   {
                (void) free(sernam);
                (void) free(comtxt);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        len = strlen(comtxt);
        *rcode = sysreq(sernam, comtxt, &len, reptxt, &arysz);
        (void) free(sernam);
        (void) free(comtxt);
/*
 * Copy back, skipping new lines.
 */
 
        for (i=0,j=0,k=1;(i<arysz)&&(k<=arylen);i++)     {
                if (reptxt[i]!= '\n')    {
                        freptxt[j++] = reptxt[i];
                }
                else {
                        j = arylen*(k++);
                }
        }
        *farysz = k;
        (void) free(reptxt);
}
#endif /* UNIXF77 */
 
#if defined(vms)
 
/*
 * Based on the "Introduction to VMS System Routines" manual for VMS 5.0
 */
 
#include <ssdef.h>      /* System services definitions                  */
#include <uaidef.h>     /* User authorization definitions               */
#include <rmsdef.h>     /* Record Management definitions                */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define cuserid DECC$CUSERID
#define free    DECC$FREE
#define malloc  DECC$MALLOC
#define strncpy  DECC$STRNCPY
#define strlen  DECC$STRLEN
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
extern int _sysreq();   /* Actual call to sysreq                        */
 
static  char    account[9+1];
static  int     accountl;
 
typedef struct {
        short   bufl;
        short   code;
        char    *buf;
        int     *retl;
} Item;
 
typedef struct  {
        int     len;
        char    *buf;
} descrip;
 
static struct   {
        Item    items[57];
        int     eol;
} itemlist = {
        {
        9,      UAI$_ACCOUNT,           account,                &accountl
        },
        0
};
 
static int
do_sysreq(sernam, comtxt, comtxtl, reptxt, arysz)
char    *sernam;        /* Service name                                 */
char    *comtxt;        /* Command text                                 */
int     *comtxtl;       /* Command text length                          */
char    *reptxt;        /* Reply text                                   */
int     *arysz;         /* Reply array size, actual size on return      */
{
        char    *p;
        char    *acct;
        descrip name_dsc;
        int rc;
        char    name[L_cuserid];
 
        acct = "        ";              /* This is the default value    */
 
        cuserid(name);
        name_dsc.len = strlen(name);
        name_dsc.buf = name;
        if ((rc = sys$getuai(0, 0, &name_dsc, &itemlist, 0, 0, 0))
                                != SS$_NORMAL)       {
                if (rc == RMS$_RNF)     {
                        acct = "        ";
                }
                if (!(rc & 0x01))       {
                        vaxc$errno = rc;
                        errno = EVMSERR;
                        acct = "        ";
                }
                else    {
                        account[accountl]='\0';
                        acct = account;
                }
        }
        else    {
                account[accountl]='\0';
                acct = account;
        }
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "calling _sysreq(%s,%s,%d,%x,%d) entered",
                  sernam, comtxt, *comtxtl, reptxt, *arysz);
        END_TRACE();
        return(_sysreq(name, acct,sernam, comtxt, *comtxtl, reptxt, arysz));
}
 
void SYSREQ(fsernam, fcomtxt, rcode,freptxt,farysz)
struct dsc$descriptor_s *fsernam;
struct dsc$descriptor_s *fcomtxt;
int     *rcode,*farysz;
struct dsc$descriptor_a *freptxt;
{
 
        char    *sernam, *comtxt, *reptxt;
        char    *p;
        int     fsernaml, fcomtxtl;
        int     len, arylen, arysz;
        register int    i,j,k;
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "SYSREQ(%x,%x,%x,%x,%d) entered",
                  fsernam, fcomtxt, rcode, freptxt, *farysz);
 
/*
 * Converting fortran arrays
 */
 
        fsernaml = fsernam->dsc$w_length;
        if ((sernam = malloc(fsernaml+1)) == NULL)   {
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(sernam, fsernam->dsc$a_pointer, fsernaml);
        sernam[fsernaml] = '\0';
        TRACE(2, "sysreq", "FORTRAN service name: %s",sernam);
 
        fcomtxtl = fcomtxt->dsc$w_length;
        if ((comtxt = malloc(fcomtxtl+1)) == NULL)   {
                free(sernam);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        strncpy(comtxt, fcomtxt->dsc$a_pointer, fcomtxtl);
        comtxt[fcomtxtl] = '\0';
        TRACE(2, "sysreq", "FORTRAN comm. text: %s",comtxt);
 
        arysz = freptxt->dsc$l_arsize;
        arylen = arysz/(*farysz);
/*
 * Blank fill the array
 */
 
        for(i=0;i<arysz;i++)      {
                *((freptxt->dsc$a_pointer)+i) = ' ';
        }
        if ((reptxt = malloc(arysz+1)) == NULL)   {
                (void) free(sernam);
                (void) free(comtxt);
                *rcode = -errno;
                END_TRACE();
                return;
        }
        len = strlen(comtxt);
        *rcode = do_sysreq(sernam, comtxt, &len, reptxt, &arysz);
        (void) free(sernam);
        (void) free(comtxt);
/*
 * Copy back, skipping new lines.
 */
 
        for (i=0,j=0,k=1;(i<arysz)&&(k<=*farysz);i++)     {
                if (reptxt[i]!= '\n')    {
                        *((freptxt->dsc$a_pointer)+(j++)) = reptxt[i];
                }
                else {
                        j = arylen*(k++);
                }
        }
        *farysz = k;
        (void) free(reptxt);
}
#endif /* vms */
