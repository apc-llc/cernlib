/*
 * $Id: fafldr.h,v 1.1.1.1 1996/03/07 15:18:25 mclareni Exp $
 *
 * $Log: fafldr.h,v $
 * Revision 1.1.1.1  1996/03/07 15:18:25  mclareni
 * Fatmen
 *
 */
/*
++++++++++++++++++++++++++FAFLDR
.type:          definitions
.language:      C
.ident:         next file in direcetory
.version:       1.0 - 90-05-19
.envir:         UNIX
.uses:
.author:        J. Shiers, Defert Ph.,
.keywords:      Directory list.
.descript:      Gives the next entry of a directory.
----------------------------------------------------------------
*/
 
#include <sys/types.h>
#include <dirent.h>
#define direct dirent
 
#define FAFLDR_OK     ((int)0)
#define FAFLDR_EOF    ((int)1)
#define FAFLDR_FAILED ((int)2)
#define FAFLDR_INVARG ((int)3)
 
 
/*extern int fafldr(char *dirnam, char *filnam, int icont);*/
extern int fafldr();
