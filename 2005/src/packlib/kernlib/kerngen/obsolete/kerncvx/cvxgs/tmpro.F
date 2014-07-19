*
* $Id: tmpro.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: tmpro.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      OPTIONS  -sa
      SUBROUTINE TMPRO (TEXT)
C
C CERN PROGLIB#         TMPRO           .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C
C     Fortran interface : print a prompt string
C
      CHARACTER    TEXT*(*)
      INTEGER*4    LGTEXT

      LGTEXT = LEN (TEXT)
      CALL TMPROI (TEXT,LGTEXT)
      RETURN
      END
#ifdef CERNLIB_CCGEN_TMPRO
#undef CERNLIB_CCGEN_TMPRO
#endif
