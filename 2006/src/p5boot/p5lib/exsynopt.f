CDECK  ID>, EXSYNOPT.
      SUBROUTINE EXSYNOPT

C-    Run nysynopt
C.    started 18-july-95

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
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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
      PARAMETER   (MASK= KMH + KME + KMS + KMI + KMM + KMX + KMY + KMZ)

      DIMENSION    MMKEEP(4)
      DATA MMKEEP   /  1,  4,  2,  4 /


      CALL INIAUX (LASTWK)
      NQUSED = 12

      CALL KROPT (IXLUN(2),MASK)
      CALL PGSIZE (0, IXLUN(2))
      IF (MOPTIO(8).NE.0)          GO TO 81

      IF (MOPTIO(19).NE.0)  THEN
          MOPTIO(19) = 0
        ELSE
          MOPTIO(3) = 1
          MOPTIO(5) = 0
        ENDIF
      MOPTIO(6) = 1
      CALL PKBYT (MOPTIO(1),MOPTIO(33),1,32,0)

      NQCPGH  = 120

      CALL UCOPY (MMKEEP, MMBANK(2,JBKKEE), 4)

C--       set MOPUPD such that all IF's are true in CCPROC

      MOPUPD = -1

C--       Get the cradle into memory

      INCRAD = 3
      IF (IQREAD.NE.0)  CALL ARRIVE (0)

C--       Complete the initialization

      CALL PINIT2
      MU_GLOB = MU_GLOB + KM5
      CALL SBIT0 (MU_GLOB,6)

C--       Do the cradle initially

      CALL SYCRAD
      IF (JANSW.NE.0)              GO TO 89

C--       Do next PAM file

   41 INCRAD = 0
      CALL SYPAM

C--       Do cradle continuation

      INCRAD = 2
      CALL SYCRAD
      IF (JANSW.EQ.0)              GO TO 41
      GO TO 89

C----         Help

   81 WRITE (IQPRNT,9081)

   89 CALL PGSIZE (1, 0)
      RETURN

 9081 FORMAT (' nysynopt  pam.car  options  read.cra  print'/
     F/' reads the "pam" file printing on "print" a numbered'
     F/' listing of the whole file, much like Nylist, but normally'
     F/' with sequence-calls expanded and actions signalled.'
     F/' The operation can be controlled in some detail by giving'
     F/' a cradle; if this is not needed "EOF" should be'
     F/' given for "read".'/
     F/' "options":  H  help - print this help information only'
     F/'             S  start - start each deck on a new page,'
     F/'                              unless T=JOIN present'
     F/'             E  eject - honour the pseudo sequence calls'
     F/'                               +SEQ, QEJECT, N=n.'
     F/'             M  missing sequences to be signalled'
     F/'             I  line numbers separate for individual PAM files'
     F/'             X  do not signal actions'
     F/'             Y  do not expand +CDE calls'
     F/'             Z  do not expand +SEQ calls'
     F/'       0 ... 4  - select page size by single digit option'
     F/'            "n" - set page size to be n > 19')
      END
