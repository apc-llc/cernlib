CDECK  ID>, M_ANA3.
      SUBROUTINE M_ANA3

C-    Content analysis stage 3:
C-    ready effective self-material for use
C-          activating delayed control lines
C-          evaluating IF selection
C-          resolving sequence calls
C-          collecting EXE bits from called sqs and IF= params
C-    ready new effective foreign material for use
C-          activating delayed control lines
C-          evaluating initial IF selection
C-    -  called also if PAM update mode  -
C.    started 25-nov-91

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      MUPASV = MU_PAT
      MUDKSV = MU_DECK
      LEVSK  = MCCSEL
      NONACT = 7
      LEVAN  = 7
      LUPAN  = 0

      LDOAN  = KQPREP + 1
      GO TO 21

C----------        Scan ordinary self-material         ---------

C--       MODEAN = 0 ordinary self  1 IF-conditional self
C--                2 +SELF,sname    3 +KEEP   4 +REPL,+ADB,+ADD

   21 CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 61
   22 IF (JCCTYP.LE.MCCNIL)        GO TO 21
      IF (JCCTYP.EQ.MCCTIT)        GO TO 43
      IF (JCCTYP.EQ.MCCXSQ)        GO TO 37

      CALL M_KRAK (1)

      JACT   = 0
      MODEAN = 0
      IF (JCCTYP.GE.MCCUSE)        GO TO 51
      IF (JCCTYP.GE.MCCDEL)        GO TO 71

      NONACT = 7
      IF (JCCTYP.EQ.MCCKEE)        GO TO 73
      IF (JCCTYP.EQ.MCCSKI)        GO TO 40
      IF (JCCTYP.GE.MCCSEL)        GO TO 41
      IF (JCCTYP.LT.MCCTRU)        GO TO 31

C----------        Handle +_IF etc

      IF (JCCBAD.NE.0)             GO TO 45
      IFSTEP = 1
      CALL M_A3IF (IFSTEP)
      IF (JCCBAD.NE.0)             GO TO 45
      IF (IFSTEP.NE.0)             GO TO 21
      IF (LDOAN.NE.0)              GO TO 22
      GO TO 61

C----------        Handle +CDE or +SEQ

C--       check +CDE, T=DUMMY

   31 IF (JCCBAD.NE.0)             GO TO 21
      MEXAN = IOR (MEXAN,MXCCIF)
      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  JCCIFV= 1

      CALL M_A3SQ
      GO TO 21

C--       link all seqences for XSQ bank

   37 MX    = LINKSQ (LDOAN,0)
      MEXAN = IOR (MEXAN,MX)
      GO TO 21

C----------        +SELF c/c seen

   40 IF (MOPUPD.EQ.0)  JCCIFV= 1 - JCCIFV
   41 MEXAN  = IOR (MEXAN,MXCCIF)
      IF (JCCBAD.NE.0)             GO TO 45
      IF (JCCIFV.NE.0)             GO TO 45
      IF (JCCTYP.NE.MCCSES)        GO TO 21

C--       handle +SELF, seqname

      MODEAN = 2
      CALL M_A3SQ
      IF (JCCBAD.NE.0)             GO TO 45
      IF (IQUEST(1).EQ.0)          GO TO 21
      GO TO 46

C--       deleted material

   43 JCCTYP = MCCPAT
   44 LEVSK  = JCCTYP
   45 IQ(LDOAN) = IOR (IQ(LDOAN), KM5)
   46 CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 61
      IF (JCCTYP.LT.LEVSK)  THEN
          IQ(LDOAN) = IOR (IQ(LDOAN), KM6)
          GO TO 46
        ENDIF
      JCCBAD = 0
      LEVSK  = MCCSEL
      GO TO 22

   48 IF (JACT.NE.1)               GO TO 45
   49 IQ(LDOAN) = IOR (IQ(LDOAN), KM5)
      GO TO 21

C----------        Control lines +USE etc

   51 NONACT = 7
      IF (JCCTYP.GE.MCCDEC)        GO TO 56
      IF (JCCBAD.NE.0)             GO TO 21
      IF (JCCTYP.GE.MCCASM)        GO TO 52
      IF (JCCTYP.GT.MCCEXE)        GO TO 53
      IF (JCCIFV.NE.0)             GO TO 49
      CALL X_USE
      GO TO 21

C--           c/l allowed only in the cradle

   52 IF (JCCTYP.EQ.MCCEOD)        GO TO 49
      IF (JCCTYP.EQ.MCCONL)        GO TO 91
      IF (LEXD.NE.LDCRAB)  THEN
          IF (JCCTYP.LE.MCCONL)    GO TO 94
          IF (INCRAD.EQ.0)         GO TO 94
        ENDIF

      IF (JCCTYP.GE.MCCPAM)        GO TO 49
   53 IF (JCCIFV.NE.0)             GO TO 49
      NEWCTL = 7
      GO TO 21

C----------        Control lines +DECK or +PATCH

   56 IF (JCCBAD.NE.0)             GO TO 44
      MEXAN  = IOR (MEXAN,MXCCIF)
      LEVAN  = 7
      NEWDEC = NEWDEC + 1
      IF (JCCIFV.NE.0)             GO TO 44

      CALL CCDOPT (1, JASM)
      IF (NDSLAT.NE.0)  THEN
          CALL M_FAIL ('Multiple data type')
          GO TO 44
        ENDIF

      CALL SBYT (JASM,IQ(LDOAN),21,6)
      GO TO 21

C----------        End of processing

   61 JANSW = 0
      MEXAN = IAND (MEXAN,15)
      IF (MEXAN.NE.0)  CALL MXOPER (MEXAN)

      IF (NQERR.NE.0)  THEN
          NVEXDK(1) = 0
          NVEXDK(5) = NVEXDK(2)
        ENDIF

      IF (NEWCTL+NEWFOR+NVEXDK(5).EQ.0)  RETURN

      JANSW = 1
      IF (INCRAD.LT.2)                 RETURN

      CALL SBYT (MUPASV, IQ(LEXP),1,18)
      IF (LEXD.NE.0)  CALL SBYT (MUDKSV, IQ(LEXD),1,18)
      CALL X_UDECK (0)
      RETURN

C----------   New foreign material being defined      ---------

C-                   JACT = 0 KEEP  1 DEL  2 REPL  3 ADB  4 ADD

   71 IF (JCCBAD.NE.0)             GO TO 45
      JACT   = JCCTYP - MCCKEE
      MODEAN = 4
      IQ(LDOAN) = IOR (IQ(LDOAN), MXCCIF)

      IF (NONACT.NE.0)  THEN
          NVIMAT(1) = 0
          LASTDK = 0
          NONACT = 0
        ENDIF

      CALL CREACT
      IF (LCRD.EQ.0)               GO TO 95

      IF (LCRD.EQ.LASTDK)  IQ(LDOAN)= IOR (IQ(LDOAN),KM18)
      LQ(LDOAN-3) = LCRD
      LASTDK = LCRD

C----              Create NIL action

      IF (JCCIFV.NE.0)  THEN
          IF (IAND(MXCCIF,NOT(IQ(LCRD))).EQ.0)  GO TO 48
          IQ(LDOAN) = IOR (IQ(LDOAN), KM8)
          NEWFOR = NEWFOR + 1
          GO TO 48
        ENDIF

C----              Accepted action

C-                   JACT = 0 KEEP  1 DEL  2 REPL  3 ADB  4 ADD

      NEWFOR = NEWFOR + 1
      IF (JACT.EQ.1)               GO TO 21
      CALL M_A3AC
      IF (LDOAN.EQ.0)              GO TO 61
      IF (JCCBAD.NE.0)             GO TO 45
      GO TO 22

C------            +KEEP definition

   73 IF (JCCBAD.NE.0)             GO TO 45
      MODEAN = 3
      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  GO TO 45
      IQ(LDOAN) = IOR (IQ(LDOAN), MXCCIF)

C--       check sequence existing

      CALL LOCKEEP (LACT)
      KACT = IQUEST(1)

C--         T=APPEND?
      IF (LACT.NE.0)  THEN
          IF (IAND(MCCPAR(JCCPT+1),KMA).EQ.0)  GO TO 45
          IF (IAND(IQ(LACT+1),KM19).NE.0)      GO TO 45
          IF (JCCIFV.NE.0)                     GO TO 45

          IQ(LACT+1)  = IOR (IQ(LACT+1),KM19)
          LQ(LDOAN-3) = -LACT
          NEWFOR = NEWFOR + 1
          GO TO 76
        ENDIF

C----              Create NIL sequence if IF deselected

      IFLNIL = 0
      IF (JCCIFV.EQ.0)                         GO TO 74
      IF (IAND(MXCCIF,NOT(MU_GLOB)).EQ.0)      GO TO 45

      LPD  = IQUEST(11)
      IF (LPD.NE.0)  THEN
          IF (IAND(MXCCIF,NOT(IQ(LPD))).EQ.0)  GO TO 45
        ENDIF

      IQ(LDOAN) = IOR (IQ(LDOAN), KM8)
      IFLNIL = KM12

C----              Accepted sequence definition

   74 NEWFOR = NEWFOR + 1
      IFLNOL = ISHFTL (IAND(MCCPAR(JCCPT+1),KMN),2)
      IXS    = MCCPAR(JCCPZ+1)

C--       we have to create the KEEP bank now to over-rule
C-        possible future re-definitions in the same deck

      CALL MQLIFT (LACT,KACT,0,JBKKEE,1)

      IQ(LACT)   = IOR (IQ(LACT), IFLNIL+IFLNOL)
      IQ(LACT+4) = IXS

      LQ(LDOAN-3) = LACT

      IF (IFLNIL.NE.0)             GO TO 45
   76 CALL M_A3KE
      IF (LDOAN.EQ.0)              GO TO 61
      IF (JCCBAD.NE.0)             GO TO 45
      GO TO 22

C-----             Trouble

   91 CALL FAILCC (0, 'Obsolete feature not implemented')
      GO TO 49

   94 CALL FAILCC (0, 'Control line not allowed here')
      GO TO 49

   95 CALL M_FAIL ('Implied target deck for action not defined')
      GO TO 48
      END
