CDECK  ID>, LOCSQ2.
      SUBROUTINE LOCSQ2 (KPAR)

C-    Slave to LOCSEQ, search one particular linear structure
C-    Input:   KPAR    search the linear structure attached at LQ(KPAR)
C-             IQUEST(11)  name index of sequence to be found
C-    Output:  IQUEST(1)   K-adr of sequence found, if found, or
C-                         K-adr in last bank searched, if not found
C-             IQUEST(4)   cummulated EXE bits
C-             IQUEST(12)  L-adr of sequence found

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
      EQUIVALENCE (KSQLOC,IQUEST(1)), (L,IQUEST(12))

      PARAMETER   (LOCID=4)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


   21 KNIL = 0
      K = KPAR
   22 L = KQFIND (IQUEST(11),LOCID,K,KSQLOC)
      IF (L.EQ.0)                  RETURN
      IF (KNIL.NE.0)               GO TO 27
      IQUEST(4) = IOR (IQUEST(4),IQ(L))
      IF (IAND(IQ(L),KM12).EQ.0)   RETURN

C--         NIL-sequence found, find true sequence

      KNIL = KSQLOC
      K    = L-1
      GO TO 22

C--         Found sequence covered by NIL, transmit EXE bits and de-link

   27 IQ(L) = IOR (IQ(L), IAND(IQUEST(4),15))
      CALL TOGARB (KNIL,0)
      GO TO 21
      END
