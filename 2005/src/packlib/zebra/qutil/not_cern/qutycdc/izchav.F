      SUBROUTINE IZCHAV (NP)

#include "zebra/zceta.inc"
#include "zebra/zkrakc.inc"
C--------------    END CDE                             --------------


      JCE = 0
      JWH = 0

   11 JWH = JWH + 1
      MWH = IQHOLK(JWH)

      DO 24  JCH=1,10
      MWH = SHIFT (MWH,6)
      JV  = MWH .AND. 63
      IQCETK(JCE+1) = IQCETA(JV+1)
   24 JCE = JCE + 1
      IF (JCE.LT.NP)               GO TO 11
      RETURN
      END
