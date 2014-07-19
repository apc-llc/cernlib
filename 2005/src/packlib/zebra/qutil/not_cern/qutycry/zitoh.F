      SUBROUTINE ZITOH (INTV,HOLL,NP)

#include "zebra/zbcd.inc"
C--------------    END CDE                             --------------
      INTEGER      INTV(99), HOLL(99)

      PARAMETER   (IBL32 = 4010020040 B)

      DO 39  JWH=1,NP
      INTW = INTV(JWH)
      MWH  = IBL32

      DO 29  JL=1,4
      MWH  = SHIFT (MWH,8)
      JV   = INTW .AND. 63
      MWH  = MWH .OR. NQHOLL(JV)
   29 INTW = SHIFTR (INTW,6)

   39 HOLL(JWH) = SHIFT (MWH,32)
      RETURN
      END
