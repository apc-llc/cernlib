*
* $Id: getenv.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: getenv.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if !defined(CERNLIB_CRAY)
/*
++++++++++++++++++++++++++++++APOGE
.type:      module
.language:  C
.ident:     Fortran interface to getenv for apollo
.version:   1.0 - 90-05-19
.envir:     DomainIX
.uses:      libc
.author: J. Shiers, Defert Ph.,
.keywords:  Environment variable
.descript:  Get the value of an environment variable
------------------------------------------------------------------
*/
/*
--  external declarations
*/
#include <stdio.h>
#include <string.h>
#include <errno.h>
extern char* calloc();
extern char* getenv();
/*
--  body
*/
#define EV_OK ((int)0)
#define EV_NOEXIST ((int)1)
#define EV_MALLOC ((int)2)
 
int fmgenv(envvar,envval,varlen,vallen)
char *envvar;
char *envval;
int varlen;
int vallen;
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.input:       envvar,varlen: fortran string descriptor -> the name of the
                      variable
.output:
.returns:     status of the call (EV_OK,EV_NOEXIST,EV_MALLOC)
.type:        function
.description: Get the value of an environment variable
------------------------------------------------------------------------------
*/
{
    char *cenvvar;
    char *cenvval;
    char *p;
    int i;
/*
--  conversion of fortran input to c
*/
    cenvvar = calloc(varlen+1, sizeof(char));
    if (cenvvar == NULL) return(EV_MALLOC);
    strncpy(cenvvar,envvar,varlen);
    cenvvar[varlen] = '\0';
    p = strchr(cenvvar,' ');
    if (p) *p = '\0';
/*
--  call to the real c function
*/
    errno = 0;
    cenvval = getenv(cenvvar);
    if (cenvvar) free(cenvvar);
/*
--  checks returned pointer
*/
    if (envval == NULL)
      return(EV_NOEXIST);
    strncpy(envval,cenvval,vallen);
    for (i = strlen(cenvval); i < vallen ; i++)
      envval[i] = ' ';
    return(EV_OK);
}
#endif
