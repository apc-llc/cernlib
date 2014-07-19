/*
 * $Id: sysreqc.c,v 1.2 1996/09/20 12:53:21 cernlib Exp $
 *
 * $Log: sysreqc.c,v $
 * Revision 1.2  1996/09/20 12:53:21  cernlib
 * have sysreq as sysreq_ on linux (?!)
 *
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
"@(#)sysreqc.c  1.9 7/14/94 CERN CN-SW/DC Frederic Hemmer";
#endif /* not lint */
 
/* sysreqc.c    C interface to the SYSREQ communication system          */
 
#include "config.h"             /* configuration parameters             */
#if !defined(vms)
#include <sysreq.h>             /* sysreq interface definitions         */
#include <serrno.h>             /* special error numbers                */
#include <trace.h>              /* tracing definitions                  */
#else
#include "sysreq.h"             /* sysreq specific definitions          */
#include "serrno.h"             /* special Error numbers                */
#include "trace.h"              /* tracing definitions                  */
#endif /* vms */
 
#include <stdio.h>              /* Standard Input/Output definitions    */
#if defined(unix) || (defined(_AIX) && defined(_IBMR2))
#if defined(vms)
#include <pwd_vms.h>
#else
#include <pwd.h>
#endif
#endif /* unix || (_AIX && _IBMR2) */
#include <errno.h>              /* Standard error numbers               */
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define cuserid DECC$CUSERID
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
static int sysreq_tracing = 0;
extern int _sysreq();   /* Actual call to sysreq                        */
extern char *getacct(); /* get account string                           */
 
int
#ifdef CERNLIB_LINUX
sysreq_(sernam, comtxt, comtxtl, reptxt, arysz)
#else
sysreq(sernam, comtxt, comtxtl, reptxt, arysz)
#endif
char    *sernam;        /* service name                                 */
char    *comtxt;        /* command text                                 */
int     *comtxtl;       /* command text length                          */
char    *reptxt;        /* reply text                                   */
int     *arysz;         /* reply array size, actual size on return      */
{
        char    *acct;  /* account string pointer                       */
        int     rc;     /* sysreq return code                           */
#if defined(unix) || (defined(_AIX) && defined(_IBMR2))
        struct  passwd  *pw;    /* password entry buffer pointer        */
#endif /* unix || (_AIX && _IBMR2) */
#if defined(vms)
        char    name[L_cuserid];        /* username buffer              */
#endif /* vms */
 
        INIT_TRACE("SYSREQ_TRACE");
        TRACE(1, "sysreq", "sysreq(%s,%s,%d,%x,%d) entered",
                 sernam, comtxt, *comtxtl, reptxt, *arysz);
 
        if (( acct = getacct()) == NULL) acct = "        ";
 
#if defined(unix) || (defined(_AIX) && defined(_IBMR2))
        pw = getpwuid(geteuid());
        rc = _sysreq(pw->pw_name, acct,sernam, comtxt, *comtxtl, reptxt,
                     arysz);
#endif /* unix || (_AIX && _IBMR2) */
#if defined(vms)
        cuserid(name);
        rc =_sysreq(name, acct, sernam, comtxt, *comtxtl, reptxt, arysz);
#endif /* vms */
        END_TRACE();
        return(rc);
}
