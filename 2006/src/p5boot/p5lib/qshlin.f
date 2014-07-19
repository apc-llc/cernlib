CDECK  ID>, QSHLIN.
      SUBROUTINE QSHLIN (KOLD,KNEW)

C-    Disconnect the linear stucture from KOLD and insert it at KNEW

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      IF (KOLD.EQ.KNEW)      RETURN
      LGO = LQ(KOLD)
      IF (LGO.EQ.0)          RETURN
      LSV = LQ(KNEW)
      LNX = LGO
   24 LL  = LNX
      LNX = LQ(LL-1)
      IF (LNX.NE.0)                GO TO 24

      LQ(KOLD) = 0
      LQ(KNEW) = LGO
      LQ(LL-1) = LSV
      RETURN
      END
