*
* $Id: getenvf.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE GETENVF (CHVAR,CHVAL,NVALR)
C
C CERN PROGLIB#         GETENVF         .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHVAR*(*), CHVAL*(*)
      CHARACTER    RESULT*64
      INTEGER*4    LOCRES
      POINTER     /LOCRES/ RESULT
      INTEGER*2    NVAR, NVAL


      NVAR = ICFIND (' ', CHVAR,1,LEN(CHVAR)) - 1
      IF (NVAR.LE.0)               GO TO 49

      CALL EV_$GET_VAR (CHVAR,NVAR,LOCRES,NVAL)
      IF (LOCRES.EQ.0)             GO TO 49
      CHVAL = RESULT(1:NVAL)
      CALL CUTOL (CHVAL(1:NVAL))
      NVALR = NVAL
      RETURN

   49 NVALR = 0
      CHVAL = ' '
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETENVF
#undef CERNLIB_CCGEN_GETENVF
#endif
#ifdef CERNLIB_TCGEN_GETENVF
#undef CERNLIB_TCGEN_GETENVF
#endif
