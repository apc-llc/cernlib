CDECK  ID>, DISAME.
      SUBROUTINE DISAME

C-    Match unmatched p/d by content, but only from the matching f/p
C.    started 6-june-94

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



      LFX = LQUSER(1)
      LFN = LQUSER(2)

      LPNL = LQ(LFN-4)
      LPNL = LQ(LPNL-1)
      IF (LPNL.EQ.0)               GO TO 27

C--       take next old patch

      LPX = LQ(LFX-4)
   24 LPX = LQ(LPX-1)
      IF (LPX.EQ.0)                GO TO 27

      LPNM = LQ(LPX-2)
      IF (LPNM.EQ.0)               GO TO 31

   25 LPNL = LPNM
      CALL DINOUN (LPX)
      CALL DINOUN (LPNM)
      IF (IAND(IQ(LPX),KM2) .NE.0)   GO TO 24
      IF (IAND(IQ(LPNM),KM2).NE.0)   GO TO 24

      CALL DISAMD (LPX,LPNM)
      GO TO 24

C--       all patches done

   27 CONTINUE
      RETURN

C--------     match the current unresolved patch on "old" by content

   31 CONTINUE
      LPN = LPNL
   32 IF (LQ(LPN-2).EQ.0)          GO TO 34
   33 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)  THEN
          LPN = LQ(LFN-4)
          LPN = LQ(LPN-1)
        ENDIF
      IF (LPN.EQ.LPNL)             GO TO 24
      GO TO 32

C----         try to match the "new" patch at LPN

C--           match by deck-names

   34 CONTINUE
      CALL DIMNAME (LPX,LPN,0,0,ISTAT)
      NLEN = IQUEST(1)
      NOLD = NQCOUNT (LQ(LPX-4))
      NNEW = NQCOUNT (LQ(LPN-4))
      NHAV = MIN (NOLD,NNEW)
      IF (NHAV.EQ.0)               GO TO 37
      IF (ISTAT.EQ.0)              GO TO 39
      IF (4*NLEN.GE.3*NHAV)        GO TO 39

C--           match by text content

   37 CALL DILOOK (LPX,LPN,1,LPX,LPN,12,80)

      IF (NFAID.LT.0)              GO TO 33

   39 LPNM = LPN
      CALL DILINK  (LPX,LPN,1,1)
      CALL DIMNAME (LPX,LPN,0,1,ISTAT)
      GO TO 25
      END
