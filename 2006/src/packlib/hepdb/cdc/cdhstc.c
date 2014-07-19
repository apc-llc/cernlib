/*
 * $Id: cdhstc.c,v 1.3 1997/09/02 13:12:44 mclareni Exp $
 *
 * $Log: cdhstc.c,v $
 * Revision 1.3  1997/09/02 13:12:44  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:07:30  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:28:39  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/28 16:24:07  mclareni
 * Hepdb, cdlib, etc
 *
 */
#if defined(CERNLIB_UNIX)
#include "hepdb/pilot.h"
#include <stdio.h>
#include <string.h>
#if (!defined(CERNLIB_NEXT))&&(!defined(CERNLIB_WINNT))&&(!defined(CERNLIB_MSDOS))
#include <sys/utsname.h>
#endif

#if defined(CERNLIB_WINNT)
#include <windows.h>
#endif

#if defined(CERNLIB_NEXT)
struct utsname{
   char sysname[9];
   char nodename[9];
   char nodeext[65-9];
   char release[9];
   char version[9];
   char machine[9];
   };
#endif

#define  MAXHOST 8
 
#if defined(CERNLIB_QXCAPT)
# ifndef CERNLIB_MSSTDCALL
   int CDHSTC(hnf)
# else
   int __stdcall CDHSTC(hnf,len_hnf)
# endif
#endif

#if defined(CERNLIB_QXNO_SC)
int cdhstc(hnf)
#endif

#if defined(CERNLIB_QX_SC)
int cdhstc_(hnf)
#endif

char *hnf;

#ifdef CERNLIB_MSSTDCALL
int len_hnf;
#endif
{
#ifdef MSDOS
        strncpy(hnf,"IBM PC",6);
#else
#ifdef WIN32
       int i = MAX_COMPUTERNAME_LENGTH+1;
       char q[MAX_COMPUTERNAME_LENGTH+1];
       GetComputerName(q,&i);
#else
        int i = MAXHOST;
        struct utsname u;
        char q[MAXHOST+1];
 
        if (uname(&u) < 0) return(-1);
        sprintf(q,"%-*s",MAXHOST,u.nodename);
#endif
        strncpy(hnf,q,i);
#endif
        return(0);
}
#endif
