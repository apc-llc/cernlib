CDECK  ID>, EXCHECK.
      SUBROUTINE EXCHECK

C-    Run nycheck
C.    started 12-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144,
     +  KM20=524288,  KM21=1048576,  KM22=2097152,  KM23=4194304,
     +  KM24=8388608, KM25=16777216, KM26=33554432, KM27=67108864,
     +  KM28=134217728, KM29=268435456, KM30=536870912, KM31=1073741824)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256,  KMJ=512,  KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19, KMT=KM20,
     +  KMU=KM21, KMV=KM22, KMW=KM23, KMX=KM24, KMY=KM25, KMZ=KM26)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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
      PARAMETER   (MASK= KMH + KMU)


      CALL INIAUX (LASTWK)

      CALL KROPT (IXLUN(2),MASK)
      CALL PGSIZE (0, IXLUN(2))
      IF (MOPTIO(8).NE.0)          GO TO 81

      NQCMAX = 90
      NQCPGH = 80
      NQJOIN = 1

C------        Load the title deck to memory      ------------

      CALL INIPAM (11, IXLUN(1), 1,0)
   21 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C----          Handle the PAM-file title line

      JDKTYP = 3
      CALL KRTITL
      JSLTTL = IQ(LARRV+10)

      IF (NQDKNO.GE.2)  THEN
          NQDKNO = ((NQDKNO-1)/100 + 1) *100
        ELSE
          NQDKNO = 0
        ENDIF

      CALL LN_GET (JSLTTL, SLLINE, 60)
      NTXT = NDSLAT

      IF (IQPRNT.NE.IQTYPE)  WRITE (IQTYPE,9024) SLLINE(1:NTXT)
 9024 FORMAT (' Read Pam file: ',A/)

C------            Loop over all decks until next title

      CALL CHDECK
      IF (JDKTYP.EQ.3)             GO TO 21

      IF (MOPTIO(21).EQ.0)         GO TO 47
      IF (N_TYP.LE.JIN_TYP)        GO TO 47
      CALL DPBLAN (1)
      WRITE (IQPRNT,9025)

      DO 27  JASM=1,N_TYP
      LASMT = LQ(LHASM-JASM)
      NDK   = IQ(LASMT+8)
      IF (NDK.EQ.0)                GO TO 27
      N = LNBLNK (CH_TYP(JASM))
      WRITE (IQPRNT,9027) NDK, CH_TYP(JASM)(1:N)
      NQUSED = NQUSED + 1
   27 CONTINUE

   47 IF (NQERR .NE.0)  CALL EXITRC (2)
      IF (NQWARN.NE.0)  CALL EXITRC (1)
      CALL DPBLAN (0)
      WRITE (IQPRNT,9049)
 9049 FORMAT (' All is well.'/)
      RETURN

C----         Help

   81 WRITE (IQPRNT,9081)
      RETURN

 9025 FORMAT ('   Table of Data types'/)
 9027 FORMAT (4X,I6,' decks of type ',A)
 9081 FORMAT (' nycheck  pam.car  options  print'/
     F/' reads the "pam" file to check all Patchy control-lines'
     F/' for syntax, reporting errors on "print".'/
     F/' "options":  H  help - print this help information only'
     F/'             U  user data-types present to cause warning exit')
      END
