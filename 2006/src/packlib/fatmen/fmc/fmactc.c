/*
 * $Id: fmactc.c,v 1.3 1997/09/02 12:56:53 mclareni Exp $
 *
 * $Log: fmactc.c,v $
 * Revision 1.3  1997/09/02 12:56:53  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:01:23  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:28  mclareni
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
# include <pwd.h>
#endif
 
#include "fmh.h"
 
#if defined(CERNLIB_QXCAPT)
#ifndef CERNLIB_MSSTDCALL
  int FMACTC(sf)
#else
  int _stdcall FMACTC(sf,len_sf)
#endif
#endif
#if defined(CERNLIB_QXNO_SC)
int fmactc(sf)
#endif
#if defined(CERNLIB_QX_SC)
int fmactc_(sf)
#endif
char *sf;
#ifdef CERNLIB_MSSTDCALL
  int len_sf;
#endif

{
        char p[MAXNAME+1];
        int ruid;
 
#ifndef WIN32
        ruid = getuid();
#else
	ruid = 0;
#endif
        sprintf(p,"%-8i",ruid);
        strncpy(sf,p,MAXNAME);
        return(ruid);
}
