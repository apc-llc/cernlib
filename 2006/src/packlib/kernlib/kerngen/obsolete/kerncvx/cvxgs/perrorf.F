*
* $Id: perrorf.F,v 1.1.1.1 1996/02/15 17:52:27 mclareni Exp $
*
* $Log: perrorf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PERRORF (TEXT)
C
C CERN PROGLIB#         PERRORF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to perror
C
      CHARACTER    TEXT*(*)

      CALL PERROR (TEXT)
      RETURN
      END
#ifdef CERNLIB_CCGEN_PERRORF
#undef CERNLIB_CCGEN_PERRORF
#endif
#ifdef CERNLIB_CCGEN_PERROI
#undef CERNLIB_CCGEN_PERROI
#endif
