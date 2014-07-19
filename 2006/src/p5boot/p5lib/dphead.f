CDECK  ID>, DPHEAD.
      SUBROUTINE DPHEAD

C-    LIST current deck/patch identifier if not yet done;
C-    if done but page almost full: page eject
C.    started 10-feb-92

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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


      IF (NQNEWH.NE.0)             GO TO 21
      IF (NQUSED.GE.NQLTOK)        GO TO 32
      RETURN

C--           start ouput for new deck

   21 NQDKPG = 0

      CHEXPD = 'p='
      CALL NA_GET (IXEXPAT,CHEXPD,3)
      IF (IXEXDEC.NE.0)  THEN
          J = NESLAT + 3
          CHEXPD(J-2:J-1) = 'd='
          CALL NA_GET (IXEXDEC,CHEXPD,J)
        ENDIF
      NCHEPD = NESLAT - 1

      IF (NVEXDK(2).EQ.0)          GO TO 24
      IF (MOPTIO(3).NE.0)          GO TO 24
      IF (NQJOIN.NE.0)             GO TO 24
      IF (NQUSED.LE.8)             GO TO 32

C--       first deck in a patch with a short blank deck?

      IF (NQLPAT.EQ.0)             GO TO 31
      IF (IXEXDEC.EQ.0)            GO TO 31
      IF (NQUSED-NQLPAT.GT.8)      GO TO 31

   24 IF (NQUSED.LT.NQLTOL)        GO TO 32

   31 NQUSED = NQLMAX
   32 CALL DPPAGE
      NQNEWH = 0
      RETURN
      END
