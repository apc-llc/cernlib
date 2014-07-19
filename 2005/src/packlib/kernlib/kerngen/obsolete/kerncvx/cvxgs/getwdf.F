*
* $Id: getwdf.F,v 1.1.1.1 1996/02/15 17:52:26 mclareni Exp $
*
* $Log: getwdf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GETWDF (TEXT)
C
C CERN PROGLIB#         GETWDF          .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to getwd
C
      COMMON /SLATE/ISL(40)
      CHARACTER    TEXT*(*)

      CALL GETCWD (TEXT)
      ISL(1) = LNBLNK (TEXT)
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETWDF
#undef CERNLIB_CCGEN_GETWDF
#endif
#ifdef CERNLIB_CCGEN_GETWDI
#undef CERNLIB_CCGEN_GETWDI
#endif
