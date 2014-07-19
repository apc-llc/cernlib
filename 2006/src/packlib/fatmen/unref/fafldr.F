*
* $Id: fafldr.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fafldr.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
/*
++++++++++++++++++++++++++FAFLDR
.type:          module
.language:      C
.ident:         next file in direcetory
.version:       1.0 - 90-05-19
.envir:         UNIX
.uses:          <dir.h>, directory procedures
.author:        J. Shiers, Defert Ph.,
.keywords:      Directory list.
.descript:      Gives the next entry of a directory.
----------------------------------------------------------------
*/
/*
     External declarations
*/
#include <stdio.h>
#include <errno.h>
#include "fafldr.h"
 
/*
     Body
*/
 
int fafldr(dirnam, filnam, icont)
char *dirnam;
char *filnam;
int icont;
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.input:       dirnam: string -> the directory name
              icont: int -> continuation flag
.output:      filnam: string -> the name of the searched file.
.returns:     status of the call (FAFLDR_OK,FAFLDR_EOF,FAFLDR_FAILED,FAFLDR_INVA
.type:        function
.description: the name of the file returned is
              - the next in the directory opened at the preceeding
              call to fafldr if icont == 1. Thus, "dirnam" is ignored.
              - the first in the directory "dirnam" if icont == 0
------------------------------------------------------------------------------
*/
{
    static DIR *dd = NULL;
    struct direct *entry;
/*
--  open directory dirnam if necessary i.e. icont == 0
*/
    filnam[0] = '\0';
    if (icont == 0) {
        if (dd != NULL) {
            (void)closedir(dd);
        }
        errno = 0;
        dd = opendir(dirnam);
        if (dd == NULL) {
            return(FAFLDR_FAILED);
        }
    }
/*
--  read next entry in directory
*/
    if (dd == NULL) {
        return(FAFLDR_INVARG);
    }
    errno = 0;
    entry = readdir(dd);
    if (entry == NULL) {
        if (errno == 0) {
            (void)closedir(dd);
            dd = NULL;
            return(FAFLDR_EOF);
        }
        else {
            (void)closedir(dd);
            dd = NULL;
            return(FAFLDR_FAILED);
        }
    }
    strcpy(filnam,entry->d_name);
    return(FAFLDR_OK);
}
