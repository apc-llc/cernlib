      SUBROUTINE IZHNUM (HOLL,INTV,NP)

      INTEGER      INTV(99), HOLL(99)

#include "zebra/q_shift.inc"

      DO 39  JWH=1,NP
   39 INTV(JWH) = ISHFTR (HOLL(JWH), 29)
      RETURN
      END
