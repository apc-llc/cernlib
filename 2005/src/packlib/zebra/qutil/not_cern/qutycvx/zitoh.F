      SUBROUTINE ZITOH (INTV,HOLL,NP)

#include "zebra/zbcd.inc"
C--------------    END CDE                             --------------
      INTEGER      INTV(99), HOLL(99)

      PARAMETER   (IBL32 = '20202020'X)

      DO 39  JWH=1,NP
      INTW = INTV(JWH)
      MWH  = 0

      DO 29  JL=1,4
      MWH  = ISHFT (MWH,8)
      JV   = INTW .AND. 63
      MWH  = MWH .OR. NQHOLL(JV)
   29 INTW = ishft (INTW,-6)

   39 HOLL(JWH) = ISHFT(MWH,32) .OR. IBL32
      RETURN
      END
