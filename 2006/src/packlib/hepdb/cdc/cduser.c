/*
 * $Id: cduser.c,v 1.3 1997/09/02 13:12:45 mclareni Exp $
 *
 * $Log: cduser.c,v $
 * Revision 1.3  1997/09/02 13:12:45  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:07:31  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:28:40  mclareni
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
#ifdef WIN32
#include <windows.h>
#else
#ifndef MSDOS
#include <pwd.h>
#endif
#endif
 
#define MAXNAME 8
 
#if defined(CERNLIB_QXCAPT)
# ifndef CERNLIB_MSSTDCALL
   int CDUSER(sf)
# else
   int __stdcall CDUSER(sf, len_sf, irc)
# endif
#endif

#if defined(CERNLIB_QXNO_SC)
int cduser(sf)
#endif
#if defined(CERNLIB_QX_SC)
int cduser_(sf)
#endif
char *sf;
#ifdef CERNLIB_MSSTDCALL
int len_sf;
int *irc;
#endif
{
#ifdef MSDOS
      lname = 8
      strncpy(sf,"IBM PC",6);
#else
        char *p;
        int lname;
        int i;
#ifdef WIN32
        i = 0;
        p = (char *) malloc(MAXNAME+1);
        while(!GetUserName(p,&lname)) {
          free(p); i++;
          p = (char *) malloc(i*MAXNAME+1);
        }
#else
        int ruid;
        struct passwd *pw;
 
        ruid = getuid();
        pw = getpwuid(ruid);
        lname = strlen(pw->pw_name);
#endif
        if (lname > MAXNAME)
                lname = MAXNAME;
#ifdef WIN32
        strncpy(sf,p,lname);
        free(p);
#else
        strncpy(sf,pw->pw_name,lname);
#endif
        for (i=lname; i < MAXNAME; i++)
                *(sf+i) = ' ';
#endif
        return(lname);
}
#endif
