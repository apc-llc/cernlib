      SUBROUTINE IZCHAV (NP)

#include "zebra/zceta.inc"
#include "zebra/zkrakc.inc"
C--------------    END CDE                             --------------
      DIMENSION    ICHTR(120)
      EQUIVALENCE (ICHTR,IQHOLK)
#endif
#if !defined(CERNLIB_EQUHOLCH)
      COMMON /ZCRAKC/ ICHTR(15)
#endif

      JCE = 0
      JWH = 0

   11 JWH = JWH + 1
      MWH = ICHTR(JWH)

      DO 24  JCH=1,8
      MWH = ISHFTC (MWH,8,64)
      JV  = MWH .AND. 255
      IQCETK(JCE+1) = IQCETA(JV+1)
   24 JCE = JCE + 1
      IF (JCE.LT.NP)               GO TO 11
      RETURN
      END
