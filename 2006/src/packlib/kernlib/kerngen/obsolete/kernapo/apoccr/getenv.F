*
* $Id: getenv.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: getenv.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE GETENV (CHVAR,CHVAL)
C
C CERN PROGLIB#         GETENV          .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHVAR*(*), CHVAL*(*)
      CHARACTER    RESULT*64
      INTEGER*4    LOCRES
      POINTER     /LOCRES/ RESULT
      INTEGER*2    NVAR, NVAL


      NVAR = LNBLNK (CHVAR)
      CALL EV_$GET_VAR (CHVAR,NVAR,LOCRES,NVAL)
      IF (LOCRES.NE.0)  THEN
          CHVAL = RESULT(1:NVAL)
          CALL CUTOL (CHVAL(1:NVAL))
        ELSE
          CHVAL = ' '
        ENDIF
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETENV
#undef CERNLIB_CCGEN_GETENV
#endif
#ifdef CERNLIB_TCGEN_GETENV
#undef CERNLIB_TCGEN_GETENV
#endif
