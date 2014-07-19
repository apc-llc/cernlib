/*
 * $Id: fmhstc.c,v 1.5 1997/10/23 13:39:38 mclareni Exp $
 *
 * $Log: fmhstc.c,v $
 * Revision 1.5  1997/10/23 13:39:38  mclareni
 * NT mods
 *
 * Revision 1.4  1997/09/02 12:56:53  mclareni
 * WINNT correction
 *
 * Revision 1.3  1997/04/17 10:28:44  mclareni
 * cc on OSF1 does not accept cpp directives starting in col 2
 *
 * Revision 1.2  1997/03/14 17:01:24  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:29  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
 * Fatmen
 *
 */
#include "fatmen/pilot.h"
#include <stdio.h>
#include <string.h>
#if !defined(CERNLIB_NEXT) && !defined(WIN32)
#include <sys/utsname.h>
#endif

#ifdef WIN32
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
 
#include "fmh.h"
 
#if defined(CERNLIB_QXCAPT)
#ifndef CERNLIB_MSSTDCALL
  int FMHSTC(hnf,htf,osf)
#else
  int _stdcall FMHSTC(hnf,len_hnt, htf,len_htf, osf,len_osf)
#endif
#endif
#if defined(CERNLIB_QXNO_SC)
int fmhstc(hnf,htf,osf)
#endif
#if defined(CERNLIB_QX_SC)
int fmhstc_(hnf,htf,osf)
#endif
char *hnf;
char *htf;
char *osf;
#ifdef CERNLIB_MSSTDCALL
  int len_hnt;
  int len_htf;
  int len_osf;
#endif

{
        char p[64];
        char q[MAXHOST+1];
        char r[MAXINFO+1];
#ifndef WIN32
        struct utsname u;
        int i;
        if (uname(&u)) return(-1);
        sprintf(q,"%-*s",MAXHOST,u.nodename);
        strncpy(hnf,q,MAXHOST);
        sprintf(r,"%-*s",MAXINFO,u.machine);
        strncpy(htf,r,MAXINFO);
        sprintf(p,"%s %s %s",u.sysname,u.release,u.version);
#else
       OSVERSIONINFO OsVersion;
       int i = MAX_COMPUTERNAME_LENGTH+1;
       char qc[MAX_COMPUTERNAME_LENGTH+1];

       OsVersion.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);

       GetVersionEx(&OsVersion);
/****************************************************************/
/*    DWORD dwMajorVersion; 				     	*/
/*    DWORD dwMinorVersion; 					*/
/*    DWORD dwBuildNumber; 					*/
/*    DWORD dwPlatformId; - Identifies the platform supported 	*/
/*                          by the operating system.  		*********/
/*                     VER_PLATFORM_WIN32s        Win32s on Windows 3.1 */
/*                     VER_PLATFORM_WIN32_WINDOWS Win32 on Windows 95	*/
/*                     VER_PLATFORM_WIN32_NT      Windows NT 		*/
/************************************************************************/

       switch (OsVersion.dwPlatformId) {
        case VER_PLATFORM_WIN32s: 
		sprintf(p,"%s %s %s","MS Windows",OsVersion.dwMajorVersion,
						  OsVersion.dwMinorVersion);
				break;
 	case VER_PLATFORM_WIN32_WINDOWS:
		sprintf(p,"%s %d %d","Windows 95",OsVersion.dwMajorVersion,
					          OsVersion.dwMinorVersion);
		break;
	case VER_PLATFORM_WIN32_NT: 
		sprintf(p,"%s %d %d","Windows NT",OsVersion.dwMajorVersion,
						  OsVersion.dwMinorVersion);
		break;
	default:
		break;
       }

       GetComputerName(q,&i);
       sprintf(q,"%-*s",MAX_COMPUTERNAME_LENGTH,qc);
       strncpy(hnf,q,MAX_COMPUTERNAME_LENGTH);
       strncpy(htf,q,MAX_COMPUTERNAME_LENGTH);

#endif
        i = strlen(p);
        if (i > MAXINFO)
                i = MAXINFO;
        strncpy(osf,p,i);

        for (; i < MAXINFO; i++)
                *(osf+i) = ' ';
        return(0);
}
