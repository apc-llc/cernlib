      SUBROUTINE IZBCDV (NP)

#include "zebra/zceta.inc"
#include "zebra/zkrakc.inc"


      DO 29  JWH=1,NP
      JV = SHIFT (IQHOLK(JWH),6) .AND. 63
   29 IQCETK(JWH) = IQCETA(JV+1)
      RETURN
      END
