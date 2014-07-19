CDECK  ID>, DISAMD.
      SUBROUTINE DISAMD (LPOLD,LPNEW)

C-    Match unmatched decks of LPOLD and LPNEW by content
C.    started 6-june-94

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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


      LPX = LPOLD
      LPN = LPNEW

      LDX = LQ(LPX-4)
      LDN = LQ(LPN-4)
      LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                RETURN

C--           take next unmatched deck on OLD

      LDNL = LDN
   42 LDX  = LQ(LDX-1)
      IF (LDX.EQ.0)                RETURN

      LU = LQ(LDX-2)
      IF (LU.NE.0)  THEN
          LDNL = LU
          GO TO 42
        ENDIF

C--           try to pair each unmatched deck on NEW in turn

      LDN = LDNL
   45 IF (LQ(LDN-2).EQ.0)          GO TO 47
   46 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)  LDN= LQ(LPN-4)
      IF (LDN.EQ.LDNL)             GO TO 42
      GO TO 45

C--           try to match the "new" deck at LDN

   47 CALL DILOOK (LDX,LDN,0,LPX,LPN,8,80)

      IF (NFAID.LT.0)              GO TO 46
      IF (NFAID.EQ.0)  IQ(LDX+6)=4

      LDNL = LDN
      CALL DILINK (LDX, LDN, 0,1)
      CALL DINOUN (LPX)
      CALL DINOUN (LPN)
      IF (IAND(IQ(LPX),KM2).NE.0)  RETURN
      IF (IAND(IQ(LPN),KM2).NE.0)  RETURN
      GO TO 42

      END
