CDECK  ID>, INIPAM.
      SUBROUTINE INIPAM (LUNU, IXFILE, IFLAG,IFLEX)

C-    Connect PAM file for auxiliaries
C-        IFLAG => 0  start PAM file, with log print if >0
C-               < 0  close the PAM file
C-
C-        IFLEX =  2  the file does not need to exist
C-                 0  fatal exit for OPEN failure
C.    started 12-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
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
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------

C----          Connect the PAM file

      IF (IFLAG.LT.0)              GO TO 41

      IN_LUN = LUNU
      IXFLUN = IXFILE
      IF (IXFLUN.EQ.0)             GO TO 91

      CALL FLINK  (IN_LUN, 2, -1, IFLEX)
      IF (IXFLUN.LT.0)             RETURN
      IQ(LARRV+1) = IN_LUN
      IQ(LARRV+2) = LUNFD
      IQ(LARRV+4) = IXFLUN
      IQ(LARRV+6) = LUNSIZ
      IQ(LARRV+7) = -2
      IF (LUNSIZ.GT.0)  IQ(LARRV+7)= 0
      JDKTYP = 3
      JDKNEX = 0
      IF (IFLAG.EQ.0)              RETURN
C--           Print log of file reading

      SLLINE(1:25) = ' ---> start reading file '
      CALL NA_GET (IXFLUN,SLLINE,26)
      NTXT = NESLAT - 1
      CALL DPBLAN (0)
      WRITE (IQPRNT,9027) SLLINE(1:NTXT)
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9027) SLLINE(1:NTXT)
 9027 FORMAT (A/)
      RETURN

C----              Disconnect the PAM file

   41 IN_DOX = -1
      CALL ARRIN
      RETURN

C----              Trouble

   91 CALL P_KILL ('No PAM file name given')
      END
