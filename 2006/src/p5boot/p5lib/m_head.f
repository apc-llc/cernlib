CDECK  ID>, M_HEAD.
      SUBROUTINE M_HEAD

C-    Content analysis between stages 2 and 3:
C-    check patch/deck is accepted or not,
C-    evaluate data type,
C-    return JANSW =  -1 skip patch,  0 skip deck,  1 process
C.    started 11-july-94

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
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
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


      JCCIFV = 0
      IFREP  = 0

      JANSW = 1
      LUPAN = 0
      LDOAN = KQPREP + 1

      CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 24
      IF (JCCTYP.LT.MCCDEC)        GO TO 24
      IF (JCCTYP.GT.MCCPAT)        GO TO 24

      CALL CCKRAK (IQ(LDOAN+1))
      IF (JCCBAD.NE.0)  THEN
          CALL M_FAIL ('Syntax error')
        ELSE
          IF (NCCPIF.NE.0)  CALL CCPROC
        ENDIF

      IF (NCCPT.NE.0)  THEN
          CALL CCDOPT (1, JD_DTN)
          IF (NQJOIN.GE.0)  NQJOIN = IAND (MCCPAR(JCCPT+2),2)
          IFREP  =                   IAND (MCCPAR(JCCPT+2),1)
        ELSE
          JD_DTN = 0
          NQJOIN = MIN(NQJOIN,0)
        ENDIF

C--       line is +DECK, name, T=type, IF=...

      IF (JCCTYP.NE.MCCDEC)        GO TO 31
      JDKTYP = 1
      IF (JD_DTN.EQ.0)  JD_DTN= JD_DTP
      IF (JCCIFV.NE.0)         GO TO 49
      GO TO 47

C--       not a c/line to be looked at (mainly update mode)

   24 JD_DTN = JD_DTP
      IF (JDKTYP.EQ.1)             GO TO 47
      GO TO 34

C--       line is +PATCH, name, T=type, IF=...

   31 JD_DTN = MAX (JD_DTN,1)
      JD_DTP = JD_DTN
      IF (JDKTYP.EQ.1)             GO TO 44

      IF (IFREP.NE.0)  IQ(LEXP+1)= IOR (IQ(LEXP+1),KM5)

   34 IF (IAND(MU_PAT,KM10).EQ.0)     GO TO 39
      IF (JCCIFV.NE.0)                GO TO 38
      IF (IAND(IQ(LEXP+1),KM4).NE.0)  GO TO 47
      IF (IAND(MU_PAT,KM5).EQ.0)      GO TO 39
      GO TO 47

   38 IQ(LEXP+1) = IOR(IQ(LEXP+1),KM1)
   39 JANSW = -1
      RETURN

C--           but the original was +DECK, ...

   44 IF (JCCIFV.NE.0)             GO TO 49

   47 IF (IAND(MU_DECK,KM5).NE.0)  RETURN
   49 JANSW = 0
      RETURN
      END
