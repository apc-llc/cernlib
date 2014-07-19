CDECK  ID>, DILOST.
      SUBROUTINE DILOST

C-    Try to find unmatched decks by name+content in foreign patches:
C-    for each "old" patch which has unmatched decks, and which is
C-    matched as a patch with a patch on "new" (ie. which will not
C-    be USE-inhibited), look at its unmatched decks and try to find
C-    them as as-yet unmatched decks in other patches on "new".
C.    started 21-june-94

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /DIFFC/ NFAID, JFAIDA, JFAIDE
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
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      LFX = LQUSER(1)
      LFN = LQUSER(2)

C------       position to the next patch on OLD

      LPX = LQ(LFX-4)
   22 LPX = LQ(LPX-1)
      IF (LPX.EQ.0)                GO TO 68
      IF (IAND(IQ(LPX),KM2).NE.0)  GO TO 22

      LPNM = LQ(LPX-2)
      IF (LPNM.EQ.0)               GO TO 22

C----         position to the next patch on NEW

      LPN = LQ(LFN-4)
   24 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)                GO TO 22
      IF (IAND(IQ(LPN),KM2).NE.0)  GO TO 24
      IF (LPN.EQ.LPNM)             GO TO 24

C--       try all "old" decks against all "new" decks of same name

      LDX = LQ(LPX-4)
   42 LDX = LQ(LDX-1)
      IF (LDX.EQ.0)                GO TO 24
      IF (LQ(LDX-2).NE.0)          GO TO 42

      LDN = LQ(LPN-4)
   44 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 42
      IF (LQ(LDN-2).NE.0)          GO TO 44
      IF (IQ(LDN+4).NE.IQ(LDX+4))  GO TO 44

C--       found a deck of the same name on "old" and "new",
C--       verify that the contents match

      CALL DILOOK (LDX,LDN,0,LPX,LPN,8,60)

      IF (NFAID.LT.0)              GO TO 42
      IF (NFAID.EQ.0)  IQ(LDX+6)=4

      CALL DILINK (LDX, LDN, 0,1)
      IQ(LDN+6) = 4 + IQ(LPNM+4)
      IQ(LPN)   = IOR (IQ(LPN),KM4)

      CALL DINOUN (LPX)
      CALL DINOUN (LPN)
      IF (IAND(IQ(LPX),KM2).NE.0)  GO TO 22
      IF (IAND(IQ(LPN),KM2).NE.0)  GO TO 24
      GO TO 42

C--       done

   68 CONTINUE
      RETURN
      END
