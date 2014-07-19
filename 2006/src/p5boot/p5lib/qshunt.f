CDECK  ID>, QSHUNT.
      SUBROUTINE QSHUNT (KOLD,KNEW)

C-    Shunt the single bank at KOLD connecting it to KNEW

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      IF (KOLD.EQ.KNEW)      RETURN
      L  = LQ(KOLD)
      IF (L.EQ.0)            RETURN

      LQ(KOLD) = LQ(L-1)
      LQ(L-1)  = LQ(KNEW)
      LQ(KNEW) = L
      RETURN
      END
