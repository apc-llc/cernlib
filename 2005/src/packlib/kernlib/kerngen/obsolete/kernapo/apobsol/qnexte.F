*
* $Id: qnexte.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: qnexte.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE QNEXTE
C
C CERN PROGLIB# Z041    QNEXTE          .VERSION KERNAPO  1.15  881020
C ORIG. 25/03/85, W. VAN LEUWEN, I. ZACHAROV, NIKHEF
C
      SAVE      MAREA
      DIMENSION MAREA(16)
      DATA  MAREA  / 16*0 /

      IF (MAREA(1).EQ.0)     GO TO 21
C
C--                RE-ENTRY
C
      CALL LONGJMP (MAREA(2),J)
C
C--                FIRST ENTRY
C
   21 MAREA(1) = 77
      CALL SETJMP (MAREA(2))
C
C--                GO DOWN
C
      CALL QNEXT
      RETURN
      END
#ifdef CERNLIB_CCGEN_QNEXTE
#undef CERNLIB_CCGEN_QNEXTE
#endif
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
