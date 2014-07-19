CDECK  ID>, TISEQ.
      SUBROUTINE TISEQ

C-    Transform special sequence calls
C.    started 26-may-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
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
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      CHARACTER       CHTTNA*10, CHTTDT*20
      COMMON /USETTC/  JTTNAM,JTTVER,JTTSLA,JTTDAT,JTTCOM
     +,                NTTNA(5),  NTTDT,  NTTNORM, NTTALL
     +,               CHTTNA(5), CHTTDT
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
     +,       IXBLADK, IXSQSP(7), NTRBLA,NMODIF, JSLTTL,   LASTTI
C--------------    End CDE              --------------------------------
      EQUIVALENCE (IFLBACK,MOPTIO(2)), (IFLCMZ,MOPTIO(3))
      CHARACTER    LINE*128
      EQUIVALENCE (LINE,SLLINE)



C----              Next bank

      LDO = KQPREP + 1
   11 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                RETURN
      IF (LQ(LDO-2).NE.0)          GO TO 11

      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LT.MCCCDE)        GO TO 11
      IF (JCCTYP.GT.MCCSEQ)        GO TO 11

      JSLOLD = IQ(LDO+1)
      CALL CCKRAK (JSLOLD)
      IF (NCCPZ.NE.1)              GO TO 11

      IXZ = MCCPAR(JCCPZ+1)
      IF (IXZ.EQ.0)                GO TO 11
      JSQ = IUCOMP (IXZ,IXSQSP,4)
      IF (JSQ.EQ.0)                GO TO 11
      NSL = 1
      GO TO (21, 31, 41, 51), JSQ


C----         doing +SEQ, DATEQQ

   21 IF (IFLCMZ.NE.0)             GO TO 24
   22 IF (IFLBACK.NE.0)            GO TO 11

      JSLNEW = LN_TO3 ('+SEQ, QDATE  .      IDATQQ = ?')
      GO TO 88

   24 IF (JD_DTD.NE.JCC_TYP)       GO TO 22
      IF (IFLBACK.NE.0)            GO TO 26
      JSLNEW = LN_TO3 ('+SEQ, QDATE  .#define IDATQQ ?')
      GO TO 88

   26 LINE = '#define IDATQQ '
C-            _:.=+=.: 1_:.=+=.: 2_:.=+
      CALL CSETDI (IQDATE, LINE,17,24)
      JSLNEW = LN_TO3 (LINE(1:24))
      GO TO 88

C----         doing +SEQ, TIMEQQ

   31 IF (IFLCMZ.NE.0)             GO TO 34
   32 IF (IFLBACK.NE.0)            GO TO 11

      JSLNEW = LN_TO3 ('+SEQ, QTIME  .      ITIMQQ = ?')
      GO TO 88

   34 IF (JD_DTD.NE.JCC_TYP)       GO TO 32
      IF (IFLBACK.NE.0)            GO TO 36
      JSLNEW = LN_TO3 ('+SEQ, QTIME  .#define ITIMQQ ?')
      GO TO 88

   36 LINE = '#define ITIMQQ '
C-            _:.=+=.: 1_:.=+=.: 2_:.=+
      CALL CSETDI (IQTIME, LINE,17,22)
      JSLNEW = LN_TO3 (LINE(1:24))
      GO TO 88

C----         doing +SEQ, VERSQQ

   41 IF (IFLCMZ.EQ.0)             GO TO 11
      IF (JD_DTD.EQ.JCC_TYP)       GO TO 44
      IF (IFLBACK.NE.0)            GO TO 42
      JSLNEW = LN_TO3 ('+SEQ, QFVSNUM  .      IVERSQ = ?')
      GO TO 88

   42 LINE   = '      IVERSQ = ' // CHTTNA(3)
      JSLNEW = LN_TO3 (LINE(1:40))
      GO TO 88

   44 IF (IFLBACK.NE.0)            GO TO 46
      JSLNEW = LN_TO3 ('+SEQ, QFVERS   .#define VERSQQ "?"')
      JSL    = LN_TO3 ('+SEQ, QFVSNUM  .#define IVERSQ ?')
      NSL    = 2
      GO TO 88

   46 LINE   = '#define VERSQQ "' // CHTTNA(2)
      N      = LENOCC (LINE(1:48))
      LINE(N+1:N+1) = '"'
      JSLNEW = LN_TO3 (LINE(1:48))
      LINE   = '#define IVERSQ ' // CHTTNA(3)
      JSL    = LN_TO3 (LINE(1:48))
      NSL    = 2
      GO TO 88

C----         doing +SEQ, VIDQQ

   51 IF (IFLCMZ.EQ.0)             GO TO 11
      IF (JD_DTD.EQ.JCC_TYP)       GO TO 54

      NSL    = 2
      JSLNEW = LN_TO3 ('      CHARACTER  VIDQQ*(48)')

      IF (IFLBACK.NE.0)            GO TO 52
      JSL = LN_TO3 ('+SEQ, QFHEAD  .      DATA VIDQQ /''@(#)?>''/')
      GO TO 88

   52 LINE   = '      DATA VIDQQ /''@(#)' //
     +          CHTTNA(1)(1:9) // CHTTNA(2) // CHTTDT
      N      = LENOCC (LINE(1:68))
      LINE(N+1:N+3) = '>''/'
      JSL = LN_TO3 (LINE(1:N+3))
      GO TO 88

   54 IF (IFLBACK.NE.0)            GO TO 56
      JSLNEW =
     +  LN_TO3 ('+SEQ, QFHEAD   .static char vidqq[] = "@(#)?>"')
      GO TO 88

   56 LINE   = 'static char vidqq[] = "@(#)' //
     +          CHTTNA(1)(1:9) // CHTTNA(2) // CHTTDT
      N      = LENOCC (LINE(1:80))
      LINE(N+1:N+2) = '>"'
      JSLNEW = LN_TO3 (LINE(1:N+2))

C----     store the replacement

   88 CALL MQLIFT (LNEW, LDO,-2, JBKPRE,2)
      IQ(LNEW+1) = JSLNEW
      IQ(LNEW+2) = NSL
      GO TO 11
      END
