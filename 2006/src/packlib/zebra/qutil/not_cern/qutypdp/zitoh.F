      SUBROUTINE ZITOH (INTV,HOLL,NP)

#include "zebra/zbcd.inc"
C--------------    END CDE                             --------------
      INTEGER      INTV(99), HOLL(99)

#include "zebra/q_andor.inc"
#include "zebra/q_shift.inc"


      DO 39  JWH=1,NP
      INTW = INTV(JWH)
      MWH  = 0

      DO 29  JL=1,4
      MWH  = ISHFTL (MWH,7)
      JV   = IAND (INTW,63)
      MWH  = IOR (MWH, NQHOLL(JV))
   29 INTW = ISHFTR (INTW,6)

   39 HOLL(JWH) = IOR (ISHFTL(MWH,8), 64)
      RETURN
      END
