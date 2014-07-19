CDECK  ID>, KQFIND.
      FUNCTION KQFIND (IT,JWORD,KGO,KFOUND)

C-    Find first bank containing IT in word JWORD
C-    search linear structure attached at LQ(KGO)
C-    return its bank-adr as value, its K-adr in KFOUND
C-    if not found:  KQFIND=0, KFOUND = L-1 of last bank

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      JW = JWORD
      L  = KGO + 1
   24 K  = L-1
      L  = LQ(K)
      IF (L.EQ.0)                  GO TO 29
      IF (IQ(L+JW).NE.IT)          GO TO 24

   29 KFOUND = K
      KQFIND = L
      RETURN
      END
