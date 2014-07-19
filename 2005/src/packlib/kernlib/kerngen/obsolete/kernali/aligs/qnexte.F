*
* $Id: qnexte.F,v 1.1.1.1 1996/02/15 17:50:53 mclareni Exp $
*
* $Log: qnexte.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:53  mclareni
* Kernlib
*
*
#include "kernali/pilot.h"
      SUBROUTINE QNEXTE
C
C CERN PROGLIB# Z041    QNEXTE          .VERSION KERNALI  1.00  900919
C ORIG. 12/10/89  K.M.STORR from Apollo version
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
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
