CDECK  ID>, SYCRAD.
      SUBROUTINE SYCRAD

C-    Process the cradle
C.    started 18-july-95

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      PARAMETER   (IXCRA=2)
      CHARACTER    MARK*4, LINE*8
      EQUIVALENCE (LINE,TEXT)

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      LARRV   = LACRAD
      NDKSAV  = NQDKNO
      NQDKNO  = 0
      NSLORG  = 1
      IXEXPAM = 0
      NCHTAG  = 0
      NQWYLDO = 0
      NVEXDK(2) = 1
      NVEXDK(6) = 1
      CALL DPBLAN (0)
      IF (INCRAD.NE.3)             GO TO 21
      IF (IQREAD.NE.0)             GO TO 27

C----              Run in no-cradle mode

      MU_GLOB = IOR (MU_GLOB, KM2)

      LINE = '+PAM.   '
      MLIAD(2) = 7
      JCCTYP   = MCCPAM
      CALL CCKRAK (1)
      CALL DOAPAM
      GO TO 77

C------      Restart the cradle after having read a PAM file

   21 IF (IQREAD.EQ.0)             GO TO 79
      CALL VZERO (IQ(LQHOLD+1),3)
      CALL ARRIVE (1)

C--       lift PAT bank for P=CRA*

      CALL CREAPD (IXCRA,-1,1)
      LPCRA = LEXP
      LEXD  = 0

      IXEXPAT = IXCRA
      IXEXDEC = IXCRA
      IXEXID  = IXCRA
      LDECO   = 0
      NQNEWH  = 1
      NQJOIN  = 1
      NSLORG  = 1
      INCRAD  = 2

   27 IF (JDKTYP.GE.6)             GO TO 79
      IF (JDKTYP.GE.4)             GO TO 72

C--------       Do next deck

   29 JDKTYP = 1
      IF (JDKNEX.EQ.0)  CALL ARRIVE (2)
      CALL DOMAPA (0)
      JSLORG = JSLZER - NSLORG
      NSLHAV = IQ(LQHOLD+3)

      LUPAN = 0
      LDOAN = KQPREP + 1
      KKM5AN = 0

C----              Next PREP bank

   31 LDOAN = LQ(LDOAN-1)
      IF (LDOAN.EQ.0)              GO TO 39

C--       MODEAN = 0 self material
C-                 1 CDE expansion
C-                 2 comment line
C-                 3 +SELF header line
C-                 4 +CDE  call line
C-                 5 foreign material
C-                 6 control line

      JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (JCCTYP.LT.MCCKEE)        GO TO 33
      IF (JCCTYP.EQ.MCCKEE)        GO TO 34
      IF (JCCTYP.EQ.MCCUSE)        GO TO 34
      IF (JCCTYP.EQ.MCCLIS)        GO TO 34
      IF (JCCTYP.LT.MCCFOR)        GO TO 33
      IF (JCCTYP.LE.MCCOP2)        GO TO 34

C----         list no-action lines

   33 JCCTYP = 0
      CALL DPLIST (0,LDOAN,LUPAN,0)
      GO TO 31

C----         Nysynopt cradle actions

   34 CALL CCKRAK (IQ(LDOAN+1))

C--       Do :  +KEEP

      IF (JCCTYP.EQ.MCCKEE)  THEN
          CALL SYKEEP
          GO TO 38
        ENDIF

C--       Do :  +USE  +LIST  +FORCE  +SUSPEND

   36 IF (JCCTYP.LE.MCCSUS)  THEN
          CALL X_USE
          GO TO 38
        ENDIF

C--       Do :  +OPTION  +PARAMETER  +SHOW

      IF (JCCTYP.LE.MCCSHO)  THEN
          CALL X_OPT
          GO TO 38
        ENDIF

      CALL P_CRASH ('SYCRAD should not reach this point')

   38 CALL DPLIST (6,LDOAN,LUPAN,1)
      GO TO 31

C----         step to the next deck

   39 INCRAD = 1
      CALL ARRNXD (1)
      IF (JDKTYP.GE.6)             GO TO 79

      NSLORG = NSLORG + NSLHAV
      IF (JDKTYP.LE.3)             GO TO 29


C------      Handle +PAM or +QUIT

   72 JSLCRA = IQ(LQHOLD+1)
      NSLCRA = IQ(LQHOLD+2)
      JTXCRA = MLIAD(JSLCRA)
      NTX    = MLIAD(JSLCRA+1) - JTXCRA - NCHNEWL
      JCCTYP = JPTYPE (TEXT(JTXCRA))

      MARK = '  + '
      CALL CCKRAK (JSLCRA)
      IF (JCCBAD.NE.0)  MARK = ' *! '

C--       print the c/line

      CALL DPLINE (NSLORG,MARK,NTX,TEXT(JTXCRA))

C--       exit if faulty c/line

      IF (JCCBAD.NE.0)             GO TO 91

C------       Accepted control line +PAM or +QUIT

      IF (JCCTYP.EQ.MCCQUI)        GO TO 79
      NSLORG = 0

C--       Digest the +PAM parameters to bank at LPAM

      CALL DOAPAM
      IF (JCCBAD.NE.0)             GO TO 91

C--       Save cradle material after +PAM, ...

      JSLCRA = JSLCRA + 1
      NSLCRA = NSLCRA - 1
      IF (NSLCRA.EQ.0)             GO TO 77

      CALL MQLIFT (LASAV, LACRAD,-2, JBKASA,1)
      IQ(LASAV+1) = JSLCRA
      IQ(LASAV+2) = NSLCRA

   77 IQ(LQHOLD+2) = 0
      IQ(LPAM+14)  = IQ(LPAM+14) + 1
      NQDKNO = NDKSAV
      JANSW  = 0
      RETURN

C--       +QUIT or EOF reached

   79 JANSW = 1
      RETURN

C----         Trouble

   91 IF (JCCTYP.EQ.MCCQUI)        GO TO 79
      CALL P_FATAL ('faulty line +PAM,...')
      END
