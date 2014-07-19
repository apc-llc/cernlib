*
* $Id: tjidle.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tjidle.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TJIDLE (NIDL)

C-    Idle time to make the timing figures sizeable

#include "zebra/quest.inc"
C--------------    End CDE                             --------------
      DIMENSION    QUEST(100)
      EQUIVALENCE (QUEST(1), IQUEST(1))


      NL = 10 * NIDL
      DO  27  JL=1,NL
      DO  24   J=1,100
      QUEST(J) = SQRT (REAL(JL+J)) / 10.
   24 CONTINUE
      CALL VBIAS (QUEST, 12., QUEST, 100)
   27 CONTINUE
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
