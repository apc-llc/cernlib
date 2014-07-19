/*
 * $Id: tmpro.c,v 1.1.1.1 1996/02/15 17:52:44 mclareni Exp $
 *
 * $Log: tmpro.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:44  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE TMPRO
  CERN PROGLIB#         TMPRO           .VERSION KERNCRY  1.16  911111
  ORIG. 20/10/91, JZ
  Fortran interface routine to print a prompt string
*/
#include <fortran.h>
#include <stdio.h>
void TMPRO(text)
      _fcd text;
{
      char *buf;
      int  blen;

      buf  = _fcdtocp(text);
      blen = _fcdlen(text);
      write (1, buf, blen);
      return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_TMPRO
#undef CERNLIB_CCGEN_TMPRO
#endif
#ifdef CERNLIB_CCGEN_TMPROI
#undef CERNLIB_CCGEN_TMPROI
#endif
