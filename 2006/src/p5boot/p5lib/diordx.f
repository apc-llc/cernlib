CDECK  ID>, DIORDX.
      SUBROUTINE DIORDX (LUP)

C-    Find and record the strings of contiguous matches
C-    (unmatched banks to not disturb the order)
C.    started 18-june-94

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


      IF (IAND(IQ(LUP),KM3).NE.0)  RETURN
      LNEW = LQ(LUP-4)
      NDIS = 0

C----         start of a new string

   22 LOLD = LQ(LNEW-2)
      LX   = LOLD
      LN   = LNEW
      NLEN = 0

C--       step to next "new"

   24 LN = LQ(LN-1)
      IF (LN.EQ.0)                 GO TO 27
      IF (LQ(LN-2).EQ.0)  THEN
          IQ(LN+6) = 1
          NLEN = NLEN + 1
          GO TO 24
        ENDIF

C--       step to next "old"

   25 LX = LQ(LX-1)
      IF (LX.EQ.0)                 GO TO 27
      IF (LQ(LX-2).EQ.0)           GO TO 25
      IF (LQ(LX-2).NE.LN)          GO TO 27

C--       string continues

      NLEN = NLEN + 1
      GO TO 24

C----         end of this string

   27 IQ(LNEW+9) = NLEN
      IF (LN.EQ.0)                 GO TO 29
      LNEW = LN
      NDIS = 7
      GO TO 22

C----         finished

   29 IF (NDIS.EQ.0)  IQ(LUP)= IOR (IQ(LUP),KM3)
      RETURN
      END
