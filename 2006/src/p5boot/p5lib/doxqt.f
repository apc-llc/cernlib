CDECK  ID>, DOXQT.
      SUBROUTINE DOXQT

C-    Execute the material according to the map
C.    started 16-jan-92

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144,
     +  KM20=524288,  KM21=1048576,  KM22=2097152,  KM23=4194304,
     +  KM24=8388608, KM25=16777216, KM26=33554432, KM27=67108864,
     +  KM28=134217728, KM29=268435456, KM30=536870912, KM31=1073741824)
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
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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

      PARAMETER    (MASKCL=KM12+KM13+KM14+KM15)

      IAND (IZV,IZW) = AND (IZV, IZW)


C!    NDONE = -1
      LUPAN = 0
      LDOAN = KQPREP + 1

C----              Next bank

   20 MODEAN = 0
   21 CALL X_NEXT
C!    NDONE = NDONE + 1
   22 IF (LDOAN.EQ.0)              GO TO 87

C--       MODEAN = 0 self material
C-                 1 CDE expansion
C-                 2 comment line
C-                 3 +SELF header line
C-                 4 +CDE  call line
C-                 5 foreign material
C-                 6 control line

      JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (JCCTYP.GE.MCCKEE)        GO TO 51
      IF (JCCTYP.EQ.MCCKIL)        GO TO 51

C----------        Self material         ----------------

      IF (NVEXDK(5).EQ.0)          GO TO 21

      KKM5AN = IAND(IQ(LDOAN),KM5)
      IF (JCCTYP.EQ.0)             GO TO 35
      IF (JCCTYP.EQ.MCCNIL)        GO TO 39
      IF (JCCTYP.LE.MCCINC)        GO TO 56
      IF (JCCTYP.LE.MCCXSQ)        GO TO 36
      IF (JCCTYP.LT.MCCSEL)        GO TO 35

C--       to here +SELF, +SKIP and fault

      IF (JCCTYP.EQ.MCCSES)        GO TO 37
      MODEAN = 3

C--       to here +_IF etc

   35 IF (NVEXDK(2).NE.0)  THEN
          CALL DPLIST (MODEAN,LDOAN,LUPAN,0)
        ENDIF

      IF (NVEXDK(1).EQ.0)          GO TO 20
      IF (KKM5AN.NE.0)             GO TO 20
      CALL DPEXE (LDOAN)
      GO TO 20

C--       sequence call

   36 IF (JCCTYP.EQ.MCCXSQ)        GO TO 38
   37 IF (NVEXDK(6).NE.0)  THEN
          MODEAN = 4
          CALL DPLIST (MODEAN,LDOAN,LUPAN,0)
        ENDIF

   38 IF (KKM5AN.NE.0)             GO TO 20
      MODEAN = 1
      CALL X_SEQ (LDOAN)
      GO TO 20

C--       comment line

   39 IF (NVEXDK(6).EQ.0)          GO TO 21
      CALL DPLIST (2,LDOAN,LUPAN,1)
      GO TO 21

C----------        New foreign material     --------------

   51 KKM5AN = IAND(IQ(LDOAN),KM5)
      IF (JCCTYP.GE.MCCUSE)        GO TO 56
      IF (JCCTYP.LT.MCCKEE)        GO TO 56

      MODEAN = 5
      IF (NVEXDK(2).NE.0)  CALL DPLIST (MODEAN,LDOAN,LUPAN,1)

      CALL DOFORG (NSTEP)
      MODEAN = 0
      IF (NSTEP.EQ.0)              GO TO 22
      GO TO 21

C----------        Lines +DECK or +PATCH    --------------

   56 MODEAN = 6
      CALL CCKRAK (IQ(LDOAN+1))
      IF (NVEXDK(6).NE.0)  THEN
          CALL DPLIST (MODEAN,LDOAN,LUPAN,1)
          IF (JCCTYP.GE.MCCPAT)  NQLPAT = NQUSED
        ENDIF

      IF (KKM5AN.NE.0)             GO TO 20
      IF (JCCTYP.LT.MCCDEC)        GO TO 61
C!    IF (NDONE.EQ.0)              GO TO 20
      IF (NVEXDK(1).EQ.0)          GO TO 20

      IF (JD_NEW.EQ.0)  CALL DPEXTM (1)

      JASM = JBYT (IQ(LDOAN),21,6)

      IF (JCCTYP.EQ.MCCPAT)  THEN
          IXEXID = MCCPAR(JCCPP+1)
          JD_DTN = MAX (JASM,1)
          JD_DTP = JD_DTN
        ELSEIF (JCCTYP.EQ.MCCDEC)  THEN
          IXEXID = MCCPAR(JCCPD+1)
          JD_DTN = JASM
          IF (JD_DTN.EQ.0)  JD_DTN= JD_DTP
        ENDIF
      GO TO 20

C----------        Control lines +USE etc   ---------------

   61 IF (JCCTYP.GE.MCCUSE)        GO TO 64

C--       Do :  +INCLUDE

      IF (JCCTYP.EQ.MCCINC)  THEN
          CALL X_INCL (0)
          GO TO 20
        ENDIF

C--       Do :  +KILL

      CALL P_FATAL (CCKORG(1:NCHCCT))

C--       Done :  +USE etc if not in CRA*

   64 IF (JCCTYP.LE.MCCEXE)  THEN
          IF (INCRAD.LT.2)         GO TO 20

C--       Do :  +USE  +EXE  +LIST  +DIVERT  +XDIV

          CALL X_USE
          GO TO 20
        ENDIF

C--       Do :  +IMITATE

      IF (JCCTYP.EQ.MCCIMI)  THEN
          CALL X_IMIT
          GO TO 20
        ENDIF

C--       Do :  +ASM

      IF (JCCTYP.EQ.MCCASM)  THEN
          CALL X_ASM
          GO TO 20
        ENDIF

C--       Done :  +UPDATE  +NAMES  +GAPS  +MORE

      IF (JCCTYP.LE.MCCMOR)        GO TO 20

C!-       Do :  +ONLY
C!
C!    IF (JCCTYP.EQ.MCCONL)  THEN
C!        CALL X_ONLY (LDOAN)
C!        GO TO 20
C!      ENDIF

C--       Do :  +FORCE  +SUSPEND

      IF (JCCTYP.LE.MCCSUS)  THEN
          CALL X_USE
          GO TO 20
        ENDIF

C--       Do :  +OPTION  +PARAMETER  +SHOW

      IF (JCCTYP.LE.MCCSHO)  THEN
          CALL X_OPT
          GO TO 20
        ENDIF

      CALL P_CRASH ('DOXQT should not reach this point')

C----              Done

   87 CONTINUE
      RETURN
      END
