CDECK  ID>, EXMERGE.
      SUBROUTINE EXMERGE

C-    Run nymerge
C.    started 11-july-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
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
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      EQUIVALENCE (LMERGE,LQUSER(1))
      CHARACTER    LINE*80, COL(80)*1
      EQUIVALENCE (LINE,COL)
      PARAMETER   (MASK= KMF + KMH + KMM + KMP + KMQ + KMU)

      DIMENSION    MMDEC(4)
      DATA MMDEC   /  1,  1,  1,  3 /


      CALL INIAUX (LASTWK)

      CALL KROPT (IXLUN(4),MASK)
      IF (MOPTIO(8).NE.0)          GO TO 81
      MOPTIO(21) = MOPTIO(21) + MOPTIO(6) + MOPTIO(13)

      CALL UCOPY (MMDEC, MMBANK(2,JBKDEC), 4)

C----         load the "merge" file

      CALL MELOAD

C----         open the "new" file

      JD_LUN  = 21
      IXFLUN  = IXLUN(3)

      IF (IXFLUN.EQ.0)
     +      CALL P_KILL ('output file name missing')

      CALL FLINK (JD_LUN, 6, -1, 0)
      JD_LUN = LUNFD

C----         start the "old" file

      CALL MQLIFT (LQHOLD, 0,7, JBKHOL,1)
      CALL MQLIFT (LARRV,LQARRV,1,JBKARR,3)
      CALL INIPAM (11,IXLUN(2),1,0)
      IFSTEP = 0
      NQDKNO = -1

C--------          ready the next "merge" deck    ---------------

   31 IF (LMERGE.EQ.0)  THEN
          IXMGPAT = -1
          GO TO 41
        ENDIF

      JSLM = IQ(LMERGE+1)
      NSLM = IQ(LMERGE+2)
      JTYP = IQ(LMERGE+3)
      IF (JTYP.EQ.1)               GO TO 32
      IF (JTYP.NE.2)               GO TO 68

      JCCTYP = MCCPAT
      CALL CCKRAK (JSLM)
      IXMGPAT = MCCPAR(JCCPP+1)
      IXMGDEC = 0
      IF (JCCBAD.NE.0)             GO TO 91
      GO TO 36

   32 JCCTYP = MCCDEC
      CALL CCKRAK (JSLM)
      IF (JCCBAD.NE.0)             GO TO 91
      IXMGPAT = MCCPAR(JCCPP+1)
      IXMGDEC = MCCPAR(JCCPD+1)

   36 LINE = 'd='
      CALL NA_GET (IXMGDEC, LINE,3)
      N = NESLAT
      IF (IXMGPAT.GT.0)  THEN
          LINE(N+1:N+2) = 'p='
          CALL NA_GET (IXMGPAT, LINE,N+3)
          N = NESLAT
        ENDIF

      IF (MOPTIO(17).EQ.0)  THEN
          CALL DPBLAN (0)
          WRITE (IQPRNT,9036) LINE(1:N-1)
        ENDIF
 9036 FORMAT (43X,' merge ',A/)

C----         read the next deck from "old"

   41 CALL ARRNXD (IFSTEP)
      IF (JDKTYP.GE.4)             GO TO 71
      IFSTEP = 1
      NQDKNO = NQDKNO + 1
      IF (JDKNEX.EQ.0)  CALL ARRIVE (2)
      JSLF = IQ(LQHOLD+1)
      NSLF = IQ(LQHOLD+3)
      LINE = ' '

      IF (JDKTYP.EQ.1)             GO TO 44
      IF (JDKTYP.EQ.2)             GO TO 43

C--       Deck header is +TITLE or start of PAM

      NQDKNO = (NQDKNO+99) / 100
      NQDKNO = NQDKNO * 100
      CALL METITL (JSLF,NSLF)
      IXEXPAT = 0
      GO TO 41

C--       Deck header is +PATCH, name

   43 JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)
      IXEXPAT = MCCPAR(JCCPP+1)
      IXEXDEC = 0
      IF (IXEXPAT.EQ.0)            GO TO 92
      CALL NA_GET (IXEXPAT, LINE,1)
      CALL CRIGHT (LINE,1,32)
      N = 39
      IFPRNT = MOPTIO(17)
      GO TO 51

C--       Deck header is +DECK, name

   44 JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)
      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)            GO TO 92
      CALL NA_GET (IXEXDEC, LINE,41)
      N = NESLAT - 1
      IFPRNT = MOPTIO(16) + MOPTIO(17)

C----         choose next deck from "old" or "merge"

   51 IF (IXMGPAT.LT.0)             GO TO 56
      IF (IXMGPAT.NE.0)  THEN
          IF (IXMGPAT.NE.IXEXPAT)   GO TO 56
        ENDIF

      IF (IXMGDEC.EQ.IXEXDEC)       GO TO 61

C----         send deck from "old"

C-  |    pname | num | . | dname
C-   1          33    38  41

   56 IF (IFPRNT.EQ.0)  THEN
          CALL CSETDI (NQDKNO, LINE,34,37)
          COL(39) = '.'
          WRITE (IQPRNT,9056) LINE(1:N)
          NQLLBL = 0
        ENDIF
 9056 FORMAT (1X,A)

      CALL MESEND (JSLF,NSLF)
      GO TO 41

C----         send deck from "merge"

   61 IF (MOPTIO(17).NE.0)         GO TO 67
      IF (MOPTIO(16).EQ.0)  THEN
          COL(39) = '-'
          WRITE (IQPRNT,9056) LINE(1:N)
          NQLLBL = 0
        ENDIF

      LINE = ' '
      IF (IXMGDEC.EQ.0)  THEN
          CALL NA_GET (IXMGPAT, LINE,1)
          CALL CRIGHT (LINE,1,32)
          N = 39
        ELSE
          CALL NA_GET (IXMGDEC, LINE,41)
          N = NESLAT - 1
        ENDIF
      CALL CSETDI (NQDKNO, LINE,34,37)
      COL(39) = '+'
      WRITE (IQPRNT,9056) LINE(1:N)
      NQLLBL = 0

   67 CALL MESEND (JSLM,NSLM)
   68 CALL TOGARB (KQUSER,0)
      GO TO 31

C------       Done

   71 CALL DPBLAN (0)
      IF (LQUSER(1).EQ.0)          RETURN

C--           "merge" file has not been exhausted

      LINE = 'd='
      CALL NA_GET (IXMGDEC, LINE,3)
      N = NESLAT
      IF (IXMGPAT.GT.0)  THEN
          LINE(N+1:N+5) = 'of p='
          CALL NA_GET (IXMGPAT, LINE,N+6)
          N = NESLAT
        ENDIF

      WRITE (IQPRNT,9074) LINE(1:N-1)
 9074 FORMAT (' ***!!! ',A,' has not been found.')

      CALL P_KILL ('the merge file has not been consumed')

C----         faulty c/l

   91 LINE = ' merge:'
C-            _:.=+=.:_1
      JSL  = JSLM
      GO TO 93

   92 LINE = ' old:'
      JSL  = JSLF

   93 CALL DPBLAN (0)
      CALL LN_GET (JSL,LINE(9:),70)
      N = NDSLAT + 8

      WRITE (IQPRNT,9093) LINE(1:N)
 9093 FORMAT (' ***!!!  faulty control line:'/3X,A)
      CALL P_KILL ('fatal')

C----         Help

   81 WRITE (IQPRNT,9081)
      RETURN

 9081 FORMAT (
     F ' nymerge  merge.car  old.car  new.car  options  print'/
     F/' Merge "merge" into "old" copying to "new" with matching'
     F/' decks from "merge" overruling their brothers on "old".'
     F/' This is a purely sequential process, the decks on "merge"'
     F/' have to be in the right order.'/
     F/' options:  U  update the PAM file titles;'
     F/'           F  update only the first PAM title;'
     F/'           M  main release: update the primary version number;'
     F/'           P  ponly - log only the patch names;'
     F/'           Q  quick - no log printing;'
     F/'           H  print this help information only.')
      END
