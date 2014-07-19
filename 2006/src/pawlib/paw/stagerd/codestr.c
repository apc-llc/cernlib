/*
 * $Id: codestr.c,v 1.2 1996/04/02 22:00:17 thakulin Exp $
 *
 * $Log: codestr.c,v $
 * Revision 1.2  1996/04/02 22:00:17  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 03/10/94  21.46.35  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * codestr.c
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    15/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/* the entries in this table must agree with the
   SGD_RET_* constants defined in pfstagerd.h - the entry number 5
   is actually one of clio error codes */
static char *pfstagerdtbl[] = {
/* 0 */     "operation successful",
            "pfstagerd : stager program exec error",
            "pfstagerd : ftp program exec error",
            "pfstagerd : file name too long",
            "pfstagerd : malformed source file name",
/* 5 */     "error trying to communicate with VM (VM down?)",
            "pfstagerd : could not open file",
            "pfstagerd : could not open pipe",
            "pfstagerd : could not find file",
            "pfstagerd : user authentication error",
/* 10 */    "pfstagerd : user has no home directory",
            "pfstagerd : could not change directory",
            "pfstagerd : could not change user file mode",
            "pfstagerd : could not change user file ownership",
            "pfstagerd : ftp connection error",
/* 15 */    "pfstagerd : ftp data transfer error",
            "pfstagerd : ftp change directory failed",
            "pfstagerd : ftp login error",
            "pfstagerd : malformed HBOOK file",
            "pfstagerd : disk file write error",
/* 20 */    "pfstagerd : CLIO communication timeout",
            "pfstagerd : terminated by signal",
            "pfstagerd : can only stage FATMEN tapes",
            "pfstagerd : maximum time for stage exceeded",
            "pfstagerd : bad tape id",
/* 25 */    "pfstagerd : fatmen program exec error",
            "pfstagerd : lock denied for file to be staged",
            "pfstagerd : remove utility exec error",
            "pfstagerd : cannot fork child",
            "pfstagerd : file consistency check failure",
/* 30 */    "pfstagerd : invalid target path name",
            "pfstagerd : staging only allowed inside PIAF home dir",
            "pfstagerd : unknown return code"
};
static char cl[] = "cliolib : unspecified error code";
static char *cliolibtbl[] = {
/* -20 */   "cliolib : message to tpwrto too long",
            cl, cl, cl, cl, cl, cl, cl, cl, cl, cl, cl, cl,
/* -7 */    "cliolib : invalid allocation parameter in VMSTAGE",
            "cliolib : network error",
            "cliolib : I/O error",
            "cliolib : end of tape on write or minidisk full",
            "cliolib : end of volume on read",
            "cliolib : hit load point in BSR/BSF",
/* -1 */    "cliolib : end of file"
};
static char vm[] = "VM : unspecified error code";
static char *vmtbl[] = {
            vm, vm, vm, vm, vm, vm, vm, vm,
/* -61 */   "VM : spool file limit exceeded",
/* -60 */   "VM : error in spooling command",
            vm, vm,
/* -57 */   "VM : error in FILEDEF command (incompatible DCB?)",
            "VM : error in ACCESS command (unformatted disk?)",
            "VM : minidisk is already linked R/W or error linking R/W",
            "VM : strange error when linking R/O",
            "VM : link to minidisk refused by BERBEROS",
            "VM : target minidisk does not exist",
            "VM : target userid does not exist",
/* -50 */   "VM : authorization failed in BERBEROS",
            vm, vm, vm, vm, vm, vm, vm, vm, vm, vm, vm, vm, vm,
/* -36 */   "VM : invalid syntax when allocating VM resource",
            vm, vm, vm, vm, vm, vm, vm,
/* -28 */   "VM : file does not exist for cliodiskr"
};
static char cd[] = "cliod : unspecified error code";
static char *cliodtbl[] = {
            cd, cd, cd, cd, cd, cd, cd, cd, cd, cd, cd, cd,
            cd, cd, cd, cd, cd, cd, cd, cd, cd, cd, cd, cd,
/* -75 */   "cliod : process interrupted by the client",
            "cliod : too many users trying to use the server",
            "cliod : network problem",
            "cliod : semaphore problem",
            "cliod : shared memory problem",
/* -70 */   "cliod : urgent condition in a socket"
};
static char vs[] = "VMSTAGE : unspecified error code";
static char *vmstagetbl[] = {
            vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs,
            vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs, vs
};

/* return code string retrieval with range checking so
   that unexpected codes don't kill us */
char *
st_retcodestr(int code)
{
  /* from pfstagerd */
  if (code >= 0 && code <= SGD_RET_LASTCODE)
    return pfstagerdtbl[code];
  /* from CLIO library */
  else if (code <= -1 && code >= -20)
    return cliolibtbl[code + 20];
  /* from VM */
  else if (code <= -28 && code >= -69)
    return vmtbl[code + 69];
  /* from cliod */
  else if (code <= -70 && code >= -99)
    return cliodtbl[code + 99];
  /* from VMSTAGE */
  else if (code <= -100 && code >= -127)
    return vmstagetbl[code + 127];
  /* strange... */
  return "unknow return code";
}
