CDECK  ID>, X_NEXT.
      SUBROUTINE X_NEXT

C-    Step to next bank in the PREP chain for DOXQT
C.    started 18-nov-93

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)


      IF (LUPAN.NE.0)              GO TO 34
   22 LDOAN = LQ(LDOAN-1)
      IF (LDOAN.EQ.0)              RETURN

C--       next PREP bank

      L = LQ(LDOAN-2)
      IF (L.NE.0)                    GO TO 31
      IF (IAND(IQ(LDOAN),KM6).NE.0)  GO TO 22
      RETURN

C----     Follow reference to foreign material

   31 LOWAN = LDOAN
      LUPAN = L

C--       list what action

      CALL DPLACT
      IF (IAND(IQ(LDOAN),KM6).NE.0)  GO TO 37

C--       Next MAT bank

      LDOAN = LUPAN - 1
   34 LDOAN = LQ(LDOAN-1)
      IF (LDOAN.EQ.0)                GO TO 37
      IF (IAND(IQ(LDOAN),KM6).NE.0)  GO TO 34
      RETURN

C--       restart on the PREP chain

   37 LDOAN = LOWAN
      LUPAN = 0
      GO TO 22
      END
