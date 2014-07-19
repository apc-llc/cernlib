CDECK  ID>, EXINDEX.
      SUBROUTINE EXINDEX

C-    Run nyindex
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
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
      PARAMETER (KQUSER=9)
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
      CHARACTER    LINE*110, LINETT*60
      EQUIVALENCE (LINE,   SLLINE)
      EQUIVALENCE (LINETT, LINE(47:))

      PARAMETER   (MASK= KMB + KMH + KMP + KMQ + KMX + KMY + KMZ)

      DIMENSION    MMPAT(4), MMDECK(4), MMKEEP(4)

      DATA MMPAT    /  0,  1,  1,  2 /
      DATA MMDECK   /  0,  2,  1,  3 /
      DATA MMKEEP   /  0,  2,  1,  1 /

      IAND (IZV,IZW) = AND (IZV, IZW)


      CALL INIAUX (LASTWK)

      CALL KROPT (IXLUN(2),MASK)
      CALL PGSIZE (0, IXLUN(2))

      MOPTIO(16) = MAX (MOPTIO(16), MOPTIO(17))
      IF (MOPTIO(8).NE.0)          GO TO 81

      NQCMAX = 100
      NQCPGH = 100

      CALL UCOPY (MMPAT,  MMBANK(2,JBKPAT), 4)
      CALL UCOPY (MMDECK, MMBANK(2,JBKDEC), 4)
      CALL UCOPY (MMKEEP, MMBANK(2,JBKKEE), 4)

      CALL INIPAM (11, IXLUN(1), 1,0)

C------        Load the title deck to memory      ------------

   21 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C----          Handle the PAM-file title line

      JDKTYP = 3
      CALL KRTITL

      IF (NQDKNO.GE.2)  THEN
          NQDKNO = ((NQDKNO-1)/100 + 1) *100
        ELSE
          NQDKNO = 0
        ENDIF

C------            Loop over all decks until next title

      CALL INDDECK
      IF (JDKTYP.EQ.3)             GO TO 21
      CALL INIPAM (0,0, -1,0)

C-------           Input finished, print tables

      CALL QTOPSY (KQUSER)
      CALL QSORTN (2,KQUSER)

      KQDECK = KQUSER + 1
      CALL QTOPSY (KQDECK)
      CALL QSORTN (2,KQDECK)
      CALL QSORTN (1,KQDECK)

C----         Print list of data types

      WRITE (IQPRNT,9025)
      NQUSED = NQUSED + 3

      DO 27  JASM=1,N_TYP
      LASMT = LQ(LHASM-JASM)
      NDK   = IQ(LASMT+8)
      IF (NDK.EQ.0)                GO TO 27
      N = LNBLNK (CH_TYP(JASM))
      WRITE (IQPRNT,9027) NDK, CH_TYP(JASM)(1:N)
      NQUSED = NQUSED + 1
   27 CONTINUE
      NQLLBL = 0

C----         Print duplicates

      CALL DPBLAN (0)
      LN = LQUSER(1)
   32 LA = LN
      LN = LQ(LA-1)
      IF (LN.EQ.0)                 GO TO 35
      IF (IQ(LA+2).NE.IQ(LN+2))    GO TO 32

      IXP  = IQ(LA+2)
      LINE = 'p='
      CALL NA_GET (IXP,LINE,3)
      N = NESLAT
      WRITE (IQPRNT,9032) LINE(1:N)
      NQWARN = NQWARN + 1
      NQUSED = NQUSED + 1
      NQLLBL = 0

   33 LN = LQ(LN-1)
      IF (LN.EQ.0)                 GO TO 35
      IF (IQ(LN+2).NE.IXP)         GO TO 32
      GO TO 33

C--       deck names

   35 CALL DPBLAN (0)
      LN = LQUSER(2)
   36 LA = LN
      LN = LQ(LA-1)
      IF (LN.EQ.0)                 GO TO 41
      IF (IQ(LA+1).NE.IQ(LN+1))    GO TO 36
      IF (IQ(LA+2).NE.IQ(LN+2))    GO TO 36

      IXD  = IQ(LA+1)
      IXP  = IQ(LA+2)
      LINE = 'p='
      CALL NA_GET (IXP,LINE,3)
      N = NESLAT
      LINE(N+1:N+2) = 'd='
      N = N + 3
      CALL NA_GET (IXD,LINE,N)
      N = NESLAT
      WRITE (IQPRNT,9032) LINE(1:N)
      NQWARN = NQWARN + 1
      NQUSED = NQUSED + 1
      NQLLBL = 0

   37 LN = LQ(LN-1)
      IF (LN.EQ.0)                 GO TO 41
      IF (IQ(LN+1).NE.IXD)         GO TO 36
      IF (IQ(LN+2).NE.IXP)         GO TO 36
      GO TO 37


C-----        Print index of patches

   41 IF (MOPTIO(24).NE.0)         GO TO 88
      NBK = NQCOUNT (LQUSER(1))

      NQUSED = MOD (NQUSED, NQLMAX)

      IF (NQUSED+NBK+4.GE.NQLMAX)  THEN
          WRITE (IQPRNT,9041)
          NQUSED = 3
        ELSE
          WRITE (IQPRNT,9042)
          NQUSED = NQUSED + 3
        ENDIF

      LP = LQUSER(1)
   42 IF (LP.EQ.0)                 GO TO 45
      NDK   = IQ(LP+1)
      IXPAT = IQ(LP+2)
      IFLG  = IAND (IQ(LP),KM6)

      LINE(1:13) = '           = '
C-                 ' if 123456 = pname
C-                  _:.=+=.: 1_:.=

      IF (NDK.LT.0)  THEN
          LINE(10:13) = '-   '
        ELSE
          CALL CSETDI (NDK,LINE,5,10)
        ENDIF

      IF (IFLG.NE.0)  LINE(2:3)= 'If'

      CALL NA_GET (IXPAT,LINE,14)
      NN = NESLAT - 1

      WRITE (IQPRNT,9008) LINE(1:NN)
      NQUSED = NQUSED + 1
      LP = LQ(LP-1)
      GO TO 42

C----         Print index of decks

   45 IF (MOPTIO(25).NE.0)         GO TO 87
      NBK = NQCOUNT (LQUSER(2))

      NQUSED = MOD (NQUSED, NQLMAX)

      IF (NQUSED+NBK+4.GE.NQLMAX)  THEN
          WRITE (IQPRNT,9045)
          NQUSED = 3
        ELSE
          WRITE (IQPRNT,9046)
          NQUSED = NQUSED + 3
        ENDIF

      LD = LQUSER(2)
   46 IF (LD.EQ.0)                 GO TO 51
      IXDEC = IQ(LD+1)
      IXPAT = IQ(LD+2)
      NDK   = IQ(LD+3)

      LINE(1:26) = '           =             .'
C-                 '    123456 = dname       .pname
C-                  _:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.:

      CALL CSETDI (NDK,LINE,5,10)

      CALL NA_GET (IXDEC,LINE,14)
      NN = NESLAT
      NN = MAX (NN,26)

      LINE(NN:NN) = '.'
      CALL NA_GET (IXPAT,LINE,NN+1)
      NN = NESLAT - 1

      WRITE (IQPRNT,9008) LINE(1:NN)
      NQUSED = NQUSED + 1
      LD = LQ(LD-1)
      GO TO 46

C----         Print index of sequence definitions

   51 IF (MOPTIO(26).NE.0)         GO TO 87
      CALL QTOPSY (KQKEEP)
      CALL QSORTN (1,KQKEEP)
      NBK = NQCOUNT (LQKEEP)

      NQUSED = MOD (NQUSED, NQLMAX)

      IF (NQUSED+NBK+4.GE.NQLMAX)  THEN
          WRITE (IQPRNT,9051)
          NQUSED = 3
        ELSE
          WRITE (IQPRNT,9052)
          NQUSED = NQUSED + 3
        ENDIF

      LZ = LQKEEP
   52 IF (LZ.EQ.0)                 GO TO 87
      IXSEQ = IQ(LZ+1)
      LD    = LQ(LZ-2)
      IF (IAND(IQ(LZ),1).EQ.0)  THEN
          IXDEC = IQ(LD+1)
          IXPAT = IQ(LD+2)
          NDK   = IQ(LD+3)
        ELSE
          IXDEC = 0
          NDK   = IQ(LD+1)
          IXPAT = IQ(LD+2)
        ENDIF

      LINE(1:41) = '           :                             '
C-                 '    123456 : zname        d=dname       .pname
C-                  _:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5

      CALL CSETDI (NDK,LINE,5,10)

      CALL NA_GET (IXSEQ,LINE,14)
      NN = NESLAT
      NN = MAX (NN,26) + 1

      LINE(NN:NN+1) = 'd='
      CALL NA_GET (IXDEC,LINE,NN+2)
      NN = NESLAT
      NN = MAX (NN,41)

      LINE(NN:NN) = '.'
      CALL NA_GET (IXPAT,LINE,NN+1)
      NN = NESLAT - 1

      WRITE (IQPRNT,9008) LINE(1:NN)
      NQUSED = NQUSED + 1
      LZ = LQ(LZ-1)
      GO TO 52

C----         Help

   81 WRITE (IQPRNT,9081)

   87 CALL PGSIZE (1, 0)
   88 IF (NQWARN.NE.0)  THEN
          WRITE (IQPRNT,9088)
          CALL EXITRC (1)
        ENDIF
      RETURN

 9008 FORMAT (A)
 9025 FORMAT (/'   Table of Data types'/)
 9027 FORMAT (4X,I6,' decks of type ',A)
 9032 FORMAT (' **!!  Warn: duplicate ',A)
 9041 FORMAT ( '1  Index of  Patches'/)
 9042 FORMAT (/'   Index of  Patches'/)
 9045 FORMAT ( '1  Index of  Decks'/)
 9046 FORMAT (/'   Index of  Decks'/)
 9051 FORMAT ( '1  Index of  Sequences defined'/)
 9052 FORMAT (/'   Index of  Sequences defined'/)
 9081 FORMAT ( ' nyindex  pam.car  options  print'/
     F/' reads the "pam" file, printing on "print" a running'
     F/' table-of-content of the patches and decks encountered,'
     F/' and at the end it prints 3 sorted indices of the patches,'
     F/' decks and sequence definitions seen.'
     F/' A "duplicate" warning is given if there are two or more'
     F/' decks of the same name in the same patch.'/
     F/' "options":  B  bare - comment fields of P/D lines not to'
     F/'                       appear in the table-of-content'
     F/'             H  help - print this help information only'
     F/'             P  patch only - suppress the deck names from'
     F/'                             the table-of-content'
     F/'             Q  quick - suppress the table-of-content'
     F/'             X  - suppress all sorted indices'
     F/'             Y  - suppress the sorted indices of decks and'
     F/'                           sequence definitions'
     F/'             Z  - suppress the sorted index of sequences'
     F/'       0 ... 4  - select page size by single digit option'
     F/'            "n" - set page size to be n > 19')
 9088 FORMAT (' **!!  There are warnings  !!**'/)
      END
