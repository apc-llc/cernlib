/*
 * $Id: getcom.c,v 1.3 1997/09/02 14:24:15 mclareni Exp $
 *
 * $Log: getcom.c,v $
 * Revision 1.3  1997/09/02 14:24:15  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/02/04 17:34:07  mclareni
 * Merge Winnt and 97a versions
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:29:16  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
 * Kernlib
 *
 */
#include "kernbit/pilot.h"
#if defined(CERNLIB_WINNT)
 #include <windows.h>
 #include <string.h>
 #include <stdlib.h>
/*
 *    SUBROUTINE GETCOM(STRING,ISLEN)
 *
 * CERN PROGLIB#    Z264 IARGC           .VERSION Microsoft C  940118
 *
 * Returns command line
 * Author: Valery Fine 94/01/18
 */
#ifndef CERNLIB_MSSTDCALL
 void getcom_(string,islen,maxlen)
#else
 void _stdcall GETCOM(string,maxlen,islen)
#endif
 char *string;
 int *islen;
 int maxlen;
 {
  if (maxlen>0) {
    char *lpCommandLine;
    lpCommandLine = GetCommandLine();
    *islen = min(strlen(lpCommandLine),maxlen);
    memcpy(string,lpCommandLine,*islen);
  }
 }
#endif
