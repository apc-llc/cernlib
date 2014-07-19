CDECK  ID>, CHDECK.
      SUBROUTINE CHDECK

C-    Check all decks until next +TITLE
C.    started 12-jan-94

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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

C--           Check file starts with +PATCH or +DECK

      IF (JCCTYP.EQ.MCCDEC)  JDKTYP= 1
      IF (JCCTYP.EQ.MCCPAT)  JDKTYP= 2

C--       set limits of control division 2

      LQCSTA(2) = LQCEND(1) + 100
      LQCEND(2) = LQCSTA(2)

C--       clear text division 3

      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      JD_DTP = 1

C------        Ready the deck parameters

   21 NQNEWH = 7
      IXEXDEC = 0
      IF (JDKTYP.EQ.3)             GO TO 47
      JSLF = IQ(LQHOLD+1)
      IF (JDKTYP.EQ.1)             GO TO 42

C--       deck header is +PATCH, name

      JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)
      IXEXPAT = MCCPAR(JCCPP+1)
      IF (IXEXPAT.EQ.0)  IXEXPAT= 1
      IXEXID = IXEXPAT

      JD_DTN = 0
      IF (JCCBAD.EQ.0)  CALL CCDOPT (1, JD_DTN)
      IF (JD_DTN.EQ.0)  JD_DTN= 1
      JD_DTP  = JD_DTN
      GO TO 44

C--       deck header is +DECK, name

   42 JSLF   = IQ(LQHOLD+1)
      JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)
      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1
      IXEXID  = IXEXDEC

   44 JD_DTN = 0
      IF (JCCBAD.EQ.0)  CALL CCDOPT (1, JD_DTN)
      IF (JD_DTN.EQ.0)  JD_DTN= JD_DTP
      LASMT   = LQ(LHASM-JD_DTN)
      NR = IQ(LASMT+8)
      IQ(LASMT+8) = NR + 1

      IF (MOPTIO(21).EQ.0)         GO TO 47
      IF (JD_DTN.LE.JIN_TYP)       GO TO 47
      IF (NR.NE.0)                 GO TO 47
      SLERRM = 'New user data-type ' // CH_TYP(JD_DTN)
      CALL FAILLN (0,0,0,JSLF,SLERRM(1:LENOCC(SLERRM)))

C--       set operation mode

   47 NVEXDK(1) = 0
      NVEXDK(2) = 1
      NVEXDK(5) = 1
      NVEXDK(6) = 1
      NQJOIN = 1

C------        Process the deck

      LEXD  = 0
      NSLORG = 0

      IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C--       analyse the contents

      CALL DOMAPA (0)

C--       process the contents

      LUPAN = 0
      LDOAN = KQPREP + 1
   54 LDOAN = LQ(LDOAN-1)
      IF (LDOAN.EQ.0)              GO TO 88

      JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (JCCTYP.EQ.0)             GO TO 54

      CALL CCKRAK (IQ(LDOAN+1))
      IF (JCCBAD.NE.0)  THEN
          CALL FAILCC (1,'syntax error')
          GO TO 54
        ENDIF

      IF (JCCWAR.EQ.0)             GO TO 54
      IF (JCCWAR.EQ.1)  CALL FAILCC (0,'hidden dot')
      IF (JCCWAR.EQ.2)  CALL FAILCC (0,'trailing comma')
      GO TO 54

C--           step to the next deck

   88 CALL ARRNXD (1)
      NQDKNO = NQDKNO + 1
      IFLGAR = 0
      IF (JDKTYP.LT.3)             GO TO 21
      RETURN
      END
