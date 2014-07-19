      SUBROUTINE IZBCDV (NP)

#include "zebra/zceta.inc"
#include "zebra/zkrakc.inc"


      DO 29  JWH=1,NP
      JV = SHIFTR (IQHOLK(JWH),56)
   29 IQCETK(JWH) = IQCETA(JV+1)
      RETURN
      END
