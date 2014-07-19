CDECK  ID>, QTOPSY.
      SUBROUTINE QTOPSY (KGO)

C-    Invert order of banks in a linear stucture

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------

      LN = LQ(KGO)
      L  = 0
   11 IF (LN.EQ.0)                 GO TO 21
      LL = L
      L  = LN
      LN = LQ(L-1)
      LQ(L-1) = LL
      GO TO 11

   21 LQ(KGO) = L
      RETURN
      END
