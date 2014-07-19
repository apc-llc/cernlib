CDECK  ID>, NQCOUNT.
      FUNCTION NQCOUNT (LGO)

C-    Count the number of banks in the linear structure

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      N  = 0
      L  = LGO
   24 IF (L.NE.0)  THEN
          N = N + 1
          L = LQ(L-1)
          GO TO 24
        ENDIF

      NQCOUNT = N
      RETURN
      END
