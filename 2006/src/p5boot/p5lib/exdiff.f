CDECK  ID>, EXDIFF.
      SUBROUTINE EXDIFF

C-    Run nydiff
C.    started 3-june-94

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
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      PARAMETER   (MASK= KMH + KMA + KMD + KMF)

      DIMENSION    MMPAT(4)
      DATA MMPAT   /  0,  4,  1,  10 /


      CALL INIAUX (LASTDI)
      LQCSTA(2) = LQCEND(1) + 100
      LQCEND(2) = LQCSTA(2)

      LQLSTA(1) = LQLEND(1) + 10
      MLIAD(LQLSTA(1)) = MLIAD(LQLEND(1)) + 2048
      LQLEND(1) = LQLSTA(1)
      LQLSTA(2) = LQLEND(1)
      LQLEND(2) = LQLSTA(2)

      CALL KROPT (IXLUN(4),MASK)
      IF (MOPTIO(8).NE.0)          GO TO 81

      CALL UCOPY (MMPAT, MMBANK(2,JBKPAT), 4)
      MMPAT(2) = 3
      CALL UCOPY (MMPAT, MMBANK(2,JBKDEC), 4)

C----         load the "old" and the "new" file

      CALL DILOAD

C----         match decks on the names only

      CALL DIPREM

C----         match unmatched P/D on contents

      CALL DISAME

C----         insert proxies for new patches

      CALL DINEWP

C----     check that 3/4 of the decks have been matched

      IF (MOPTIO(6).EQ.0)  THEN
          NDK = MIN (NOBJTT(1,1), NOBJTT(1,2))
          IF (4*NOBJMAT.LT.3*NDK)
     +        CALL P_KILL ('Less than 3/4 of the decks match')
        ENDIF

C----         prepare for the re-ordering necessary

      CALL DIORD

C----         find lost decks in foreign patches

      CALL DILOST

C----         make Patchy cradle

      CALL DICRAD

C----         make Wylbur re-ordering cradle

      CALL DIWYLB
      CALL DPBLAN (0)

C----         done

      IF (NQERR.NE.0)  THEN
          WRITE (IQPRNT,9067)
          CALL EXITRC (2)
        ENDIF

      IF (NQWARN.NE.0)  THEN
          WRITE (IQPRNT,9068)
          CALL EXITRC (1)
        ENDIF
      RETURN

 9067 FORMAT (' ***!!!  There are errors  !!!***'/)
 9068 FORMAT (' **!!  There are warnings  !!**'/)

C----         Help

   81 WRITE (IQPRNT,9081)
      WRITE (IQPRNT,9082)
      RETURN

 9081 FORMAT (
     F ' nydiff  old.car  new.car  diff.ucra  options  print'/
     F/' compare "old" and "new" to derive the cradles which'
     F/' will give "new" when applied to "old":'/
     F/'   diff.ucra  to update "old" to "temp" using nypatchy;'
     F/'   diff.uexe  to re-order "temp" to "new" using Wylbur.'/
     F/' options:  A  anyway: operate even if there are too few'
     F/'                      matching patch names initially'
     F/'           F  force operation even if less than 3/4 of'
     F/'                    the decks have been matched'
     F/'           H  print this help information only.')
 9082 FORMAT (
     F/' A shell script to make "new" from "old" using the'
     F/' cradles obtained from nydiff would look like this:'/
     F/'   nypatchy  -  new.car  .go  <</'
     F/'   +UPDATE.'
     F/'   +USE, T=EXE.'
     F/'   +PAM, T=ATTACH       .diff.ucra'
     F/'   +PAM, T=ATT, UPDATE  .old.car'
     F/'   +QUIT.'
     F/'   /'/
     F/'   useb  new.car  <</'
     F/'   @diff.uexe'
     F/'   save,unn,replace'
     F/'   quit'
     F/'   /')
      END
