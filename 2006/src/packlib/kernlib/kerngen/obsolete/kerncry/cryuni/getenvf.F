*
* $Id: getenvf.F,v 1.1.1.1 1996/02/15 17:52:42 mclareni Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:42  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE GETENVF (NAME, TEXT)
C
C CERN PROGLIB# Z265    GETENVF         .VERSION KERNCRY  1.17  920229
C ORIG. 17/10/91, JZ
C Fortran interface routine to getenv
C
      COMMON /SLATE/ISL(40)
      CHARACTER*(*)  NAME, TEXT
      INTEGER        GETENV

      LGN = LNBLNK (NAME)
      ISTAT = GETENV (NAME(1:LGN),TEXT)
      IF (ISTAT.NE.0)  THEN
          ISL(1) = LNBLNK(TEXT)
        ELSE
          ISL(1) = 0
          TEXT   = ' '
        ENDIF
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETENVF
#undef CERNLIB_CCGEN_GETENVF
#endif
#ifdef CERNLIB_CCGEN_GETENI
#undef CERNLIB_CCGEN_GETENI
#endif
