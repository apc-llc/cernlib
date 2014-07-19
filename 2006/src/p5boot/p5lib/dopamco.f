CDECK  ID>, DOPAMCO.
      SUBROUTINE DOPAMCO

C-    Connect the next multi-PAM file
C.    started 19-July-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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

      IAND (IZV,IZW) = AND (IZV, IZW)


C----          Garbage collection

      CALL MQSHIFT

      JDKTYP = 3
      JDKNEX = 0

C----          Connect the PAM file

      LARRV  = LQARRV

      IN_LUN = IQ(LPAM+1)
      LABEL  = IQ(LPAM+3)
      MOPT   = IQ(LPAM+6)
      IXFLUN = IQ(LPAM+8)

      IFLATT  = IAND (MOPT,KM1)
C     IFLHOL  = IAND (MOPT,KM2)
      IFLRES  = IAND (MOPT,KM3)
      IFLUPD  = IAND (MOPT,KM4)
      IFLREW  = 1

      IF (IFLATT.NE.0)             GO TO 23

C----          Use connected file

      LUNPRE = IQ(LARRV+1)
      IXFLUN = IQ(LARRV+4)
      IF (LUNPRE.EQ.0)             GO TO 23

      IF (IFLRES.NE.0)  THEN
          IFLREW = 0
          SLLINE(1:12) = ' ---> resume'
          J = 12
        ELSE
          SLLINE(1:13) = ' ---> restart'
          J = 13
        ENDIF
      GO TO 24

C----          Attach the file

   23 CALL FLINK  (IN_LUN, 2, -1, 0)
      IQ(LARRV+2) = LUNFD
      IQ(LARRV+4) = IXFLUN
      IQ(LARRV+6) = LUNSIZ
      IQ(LARRV+7) = -2
      IF (LUNSIZ.GT.0)  IQ(LARRV+7)= 0
      SLLINE(1:11) = ' ---> start'
      J = 11

C--           Print log of file reading

   24 SLLINE(J+1:J+14) = ' reading file '
      J = J + 15
      CALL NA_GET (IXFLUN,SLLINE,J)
      NTXT = NESLAT - 1
      CALL DPBLAN (0)
      WRITE (IQPRNT,9027) SLLINE(1:NTXT)
      NQUSED = NQUSED + 2
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9027) SLLINE(1:NTXT)
 9027 FORMAT (A/)

C--       finalize the ARRV bank

      IQ(LARRV+1) = IN_LUN
      IQ(LARRV+3) = LABEL
      IQ(LARRV+12) = 0

C--       rewind unless option T=RESUME

      IF (IFLREW.NE.0)  THEN
          IN_DOX = 0
          CALL ARRIN
          NQWYL = 0
        ENDIF

      RETURN
      END
