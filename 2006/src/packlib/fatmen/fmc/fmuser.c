/*
 * $Id: fmuser.c,v 1.3 1997/09/02 12:56:55 mclareni Exp $
 *
 * $Log: fmuser.c,v $
 * Revision 1.3  1997/09/02 12:56:55  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:01:25  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:30  mclareni
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
#endif
 
#include "fmh.h"
 
#if defined(CERNLIB_QXCAPT)
# ifdef CERNLIB_MSSTDCALL
  int _stdcall FMUSER(sf,len_sf)
#else
  int FMUSER(sf)
#endif
#endif
#if defined(CERNLIB_QXNO_SC)
int fmuser(sf)
#endif
#if defined(CERNLIB_QX_SC)
int fmuser_(sf)
#endif
char *sf;
#ifdef CERNLIB_MSSTDCALL
   int len_sf;
#endif
{
        char *p;
        int ruid;
        struct passwd *pw;
        int lname;
        int i;
#ifndef WIN32 
        ruid = getuid();
        pw = getpwuid(ruid);
        lname = strlen(pw->pw_name);
        if (lname > MAXNAME)
                lname = MAXNAME;
        strncpy(sf,pw->pw_name,lname);
        for (i=lname; i < MAXNAME; i++)
                *(sf+i) = ' ';
#endif
        return(lname);
}
