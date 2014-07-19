/*
 * $Id: iargv.c,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
 *
 * $Log: iargv.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE IARGV
  CERN PROGLIB#         IARGV           .VERSION KERNCRY  1.14  910318
  ORIG. 28/04/88  FCA
*/
#include <sys/types.h>
#include <fortran.h>
extern char **_argv;
IARGV(jarg,gotext)
      _fcd gotext;
      int *jarg;
{
      int i, j, ilen;
      char *arg = _argv[*jarg];
      char *cp = _fcdtocp(gotext);
      unsigned len = _fcdlen(gotext);
      for(ilen=0 ; ilen < len && (cp[ilen]=arg[ilen]) != '\0'; ilen++);
      return(ilen);
}
/*> END <----------------------------------------------------------*/
