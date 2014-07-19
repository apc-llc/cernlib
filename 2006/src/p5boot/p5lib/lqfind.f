CDECK  ID>, LQFIND.
      FUNCTION LQFIND (IT,JWORD,LGO)

C-    Find first bank containing IT in word JWORD
C-    in the linear structure supported by LGO;
C-    return its bank-adr as value, if not found:  LQFIND=0

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      JW = JWORD
      L  = LGO
   24 IF (L.EQ.0)                  GO TO 29
      IF (IQ(L+JW).EQ.IT)          GO TO 29
      L  = LQ(L-1)
      GO TO 24

   29 LQFIND = L
      RETURN
      END
