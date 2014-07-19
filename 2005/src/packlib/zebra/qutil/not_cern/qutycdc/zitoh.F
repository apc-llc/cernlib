      SUBROUTINE ZITOH (INTV,HOLL,NP)

#include "zebra/zbcd.inc"
C--------------    END CDE                             --------------
      INTEGER      INTV(99), HOLL(99)

      PARAMETER   (IBL36 = 0"555555555555")

      DO 39  JWH=1,NP
      INTW = INTV(JWH)
      MWH  = IBL36

      DO 29  JL=1,4
      MWH  = SHIFT (MWH,6)
      JV   = INTW .AND. 63
      MWH  = MWH .OR. NQHOLL(JV)
   29 INTW = SHIFT (INTW,54)

   39 HOLL(JWH) = SHIFT (MWH,36)
      RETURN
      END
