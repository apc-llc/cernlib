      SUBROUTINE IZHNUM (HOLL,INTV,NP)

      INTEGER      INTV(99), HOLL(99)


      DO 39  JWH=1,NP
   39 INTV(JWH) = ishft (HOLL(JWH),-56)
      RETURN
      END
