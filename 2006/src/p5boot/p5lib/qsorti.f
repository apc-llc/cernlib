CDECK  ID>, QSORTI.
      SUBROUTINE QSORTI (JWORD,KGO)

C-    Sort banks at KGO for words IQ(L+JWORD) to be in increasing order

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------

      LL = LQ(KGO)
      IF (LL.EQ.0)           RETURN
      JW = JWORD

   11 LN = LQ(LL-1)
      IF (LN.EQ.0)           RETURN
      IF (IQ(LN+JW).LT.IQ(LL+JW))  GO TO 21
      LL = LN
      GO TO 11

C--                BANK  LN  OUT OF SEQUENCE

   21 LQ(LL-1)= LQ(LN-1)
      K = KGO

   24 L = LQ(K)
      IF (IQ(LN+JW).LT.IQ(L+JW))   GO TO 29
      K = L-1
      GO TO 24

C--                PLACE FOR BANK  LN  FOUND

   29 LQ(LN-1)= L
      LQ(K)   = LN
      GO TO 11
      END
