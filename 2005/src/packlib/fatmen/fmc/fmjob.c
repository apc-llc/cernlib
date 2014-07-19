/*
 * $Id: fmjob.c,v 1.3 1997/09/02 12:56:54 mclareni Exp $
 *
 * $Log: fmjob.c,v $
 * Revision 1.3  1997/09/02 12:56:54  mclareni
 * WINNT correction
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
#ifndef WIN32
#include <pwd.h>
#else
#include <windows.h>
#endif
 
#include "fmh.h"
 
#if defined(CERNLIB_QXCAPT)
#ifndef CERNLIB_MSSTDCALL
  int FMJOB(sf)
#else
  int _stdcall FMJOB(sf, len_sf)
#endif
#endif

#if defined(CERNLIB_QXNO_SC)
int fmjob(sf)
#endif
#if defined(CERNLIB_QX_SC)
int fmjob_(sf)
#endif
char *sf;
#ifdef CERNLIB_MSSTDCALL
  int len_sf;
#endif
{
        int ruid;
#ifndef WIN32
        struct passwd *pw;
#endif
        int lname;
        int i;
#ifndef WIN32 
        ruid = getuid();
        pw = getpwuid(ruid);
        lname = strlen(pw->pw_name);
        if (lname > MAXNAME)
                lname = MAXNAME;
        strncpy(sf,pw->pw_name,lname);
#else
        i = 0;
        GetUserName(sf,&lname);
#endif
        *(sf+lname++) = '-';
        sprintf(sf+lname,"%-d",getpid());

        for (lname = i = strlen(sf); i < MAXJOBN; i++)
                *(sf+i) = ' ';
        return(lname);
}
