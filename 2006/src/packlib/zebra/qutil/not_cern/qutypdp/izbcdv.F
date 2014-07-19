      SUBROUTINE IZBCDV (NP)

#include "zebra/zceta.inc"
#include "zebra/zkrakc.inc"


      DO 29  JWH=1,NP
      JV = ISHFT (IQHOLK(JWH),-29)
   29 IQCETK(JWH) = IQCETA(JV+1)
      RETURN
      END
