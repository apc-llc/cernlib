*
* $Id: tmproi.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: tmproi.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE TMPROI
  CERN PROGLIB#         TMPROI          .VERSION KERNCVX  1.08  910806
  ORIG. 06/08/91, JZ
  Fortran interface routine to print a prompt string
      CALL TMPRO (TEXT)
*/
#include <unistd.h>

void tmproi_(text, ant)
      char *text;
      int  *ant;
{
      write (1, text, *ant);
      return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_TMPROI
#undef CERNLIB_CCGEN_TMPROI
#endif
