*
* $Id: fafndf.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fafndf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_APOLLO)
/*
++++++++++++++++++++++++++++++APONF
.type:          module
.language:      C
.ident:         Fortran interface to nxfilindir for apollo
.version:       1.0 - 90-05-19
.envir:         DomainIX
.uses:          nxfldr (C standard equivalent)
.author:        J. Shiers, Defert Ph.,
.keywords:      Directory list.
.descript:      Gives the next entry of a directory.
------------------------------------------------------------------
*/
/*
--  external declarations
*/
#include <string.h>
#include "fafldr.h"
/*
--  body
*/
 
int fafndf(dirnam,filnam,icont,dirlen,fillen)
char *dirnam;
char *filnam;
int *icont;
int dirlen;
int fillen;
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.input:       dirnam,dirlen: fortran string descriptor -> the directory name
              icont: int -> continuation flag
.output:      filnam,fillen: fortran string descriptor -> the name of the
                      searched file.
.returns:     status of the call (NXFLDR_OK,NXFLDR_EOF,NXFLDR_FAILED,
              NXFLDR_INVARG)
.type:        function
.description: the name of the file returned is
              - the next in the directory opened at the preceeding
              call to nxfldr if icont == 1. Thus, "dirnam" is ignored.
              - the first in the directory "dirnam" if icont == 0
------------------------------------------------------------------------------
*/
{
    char direct[MAXNAMLEN + 1];
    char nextfile[MAXNAMLEN + 1];
    int status,i;
    char *p;
/*
--  conversion of fortran input to c
*/
    if (*icont == 0) {
      strncpy(direct,dirnam,dirlen);
      direct[dirlen] = '\0';
      p = strchr(direct,' ');
      if (p) *p = '\0';
    }
/*
--  call to the real c function
*/
    status = fafldr(direct,nextfile,*icont);
/*
--  conversion of c output to fortran
*/
    if (status == 0) {
        strncpy(filnam,nextfile,fillen);
        for (i = strlen(nextfile); i < fillen ; i++)
           filnam[i] = ' ';
    }
    return(status);
}
#endif
