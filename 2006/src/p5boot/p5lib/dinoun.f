CDECK  ID>, DINOUN.
      SUBROUTINE DINOUN (LUP)

C-    Check there are no unmatched banks depending on LUP left
C.    started 17-june-94

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
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      IF (IAND(IQ(LUP),KM2).NE.0)  RETURN

      L = LQ(LUP-4)
   24 L = LQ(L-1)
      IF (L.NE.0)  THEN
          IF (LQ(L-2).EQ.0)        RETURN
          GO TO 24
        ENDIF

      IQ(LUP) = IOR (IQ(LUP),KM2)
      RETURN
      END
