CDECK  ID>, ACSORT.
      SUBROUTINE ACSORT

C-    Sort the  action banks for current deck, mark clash levels
C-    Called from DODECK when processing of a new deck is started

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------
      DIMENSION    MBIAS(12)

      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

      DATA  MBIAS  /  5,  5,  3,  1,   0, 0, 0, 0, 4,  4,  2,  0 /
C-                   DEL REP ADB ADD              DEL REP ADB ADD
C-                     0   1   2   3   4  5  6  7   8   9  10  11

      IF (LEXD.EQ.0)               RETURN
      KACTEX = LEXD-2
      L = LQ(KACTEX)
      IF (L.EQ.0)                  RETURN

      CALL QTOPSY (KACTEX)

C--       Construct new target line numbers:  LIIN = 8*LI + bias

      L  = LQ(KACTEX)
   24 JACT = JBYT (IQ(L),9,4)
      IQ(L+4) = ISHFTL(IQ(L+4),3) + MBIAS(JACT+1)
      L = LQ(L-1)
      IF (L.NE.0)                  GO TO 24

C--       Sort the list for starting target line numbers

      CALL QSORTI (4,KACTEX)

C------            Scan for clash

      LBKF = KACTEX + 1

C--       new first bank

   32 LBKF  = LQ(LBKF-1)
      IF (LBKF.EQ.0)               GO TO 41
      JACTF = JBYT (IQ(LBKF),9,4)
      IF (JACTF.GE.8)              GO TO 32
      LBKN  = LQ(LBKF-1)
      IF (LBKN.EQ.0)               GO TO 41
      IF (JACTF.LT.2)              GO TO 34

      IF (IQ(LBKF+4).NE.IQ(LBKN+4))   GO TO 32
      IQ(LBKF) = IOR (IQ(LBKF), KM14)
      IQ(LBKN) = IOR (IQ(LBKN), KM14)
      GO TO 32

C--       and its next banks

   34 JTGFE = ISHFTL (IQ(LBKF+5),3) + 7

   36 IF (IQ(LBKN+4).GT.JTGFE)     GO TO 32

      JACTN = JBYT (IQ(LBKN),9,4)
      IF (JACTN.GE.8)              GO TO 37

      IF (IFLMERG.EQ.0)  THEN
          MSKF = KM14
          IF (LBKF.GT.LBKN)  MSKF= KM15
          MSKN = KM13 + MSKF
        ELSE
          MSKF = KM15
          MSKN = KM15
        ENDIF

      IQ(LBKF) = IOR (IQ(LBKF), MSKF)
      IQ(LBKN) = IOR (IQ(LBKN), MSKN)
   37 LBKN  = LQ(LBKN-1)
      IF (LBKN.EQ.0)               GO TO 32
      GO TO 36

C------            Reset the line numbers

   41 L = LQ(KACTEX)
   44 IQ(L+4) = ISHFTR(IQ(L+4),3)
      L = LQ(L-1)
      IF (L.NE.0)                  GO TO 44
      RETURN
      END
