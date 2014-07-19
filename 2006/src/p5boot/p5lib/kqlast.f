CDECK  ID>, KQLAST.
      FUNCTION KQLAST (KGO)

C--   Find the end of a linear structure attached to LQ(KGO)

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      L = KGO + 1
   24 K = L-1
      L = LQ(K)
      IF (L.NE.0)                  GO TO 24
      KQLAST= K
      RETURN
      END
