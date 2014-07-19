CDECK  ID>, DPLIST.
      SUBROUTINE DPLIST (MODE,LDO,LACT,IFLSTA)

C-    LIST output of next lot for DOXQT
C-    MODE  = 0 self material
C-            1 CDE expansion called from X_SEQ
C-            2 comment line
C-            3 +SELF header line
C-            4 +CDE    call line
C-            5 foreign material to be stored
C-            6 control line
C-    LDO     bank holding the lines to be listed
C-    LACT    if = zero:  material of this deck
C-            if /=zero:  ACT bank driving the material into this deck
C-    IFLSTA  if = zero:  material group continued
C-            if > zero:  start of a material group
C-                        = 1: basic level, > 1: higher levels
C-                        (used only for MODEAN = 1 and 5)
C-    also: JCCTYP in /CCPARA/ is the type of the first line in LDO
C.    started 21-jan-92

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
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
C--------------    End CDE              --------------------------------
      CHARACTER    MASKF*4, MASKL*4, MASKK(4)*4
      DATA  MASKK  / ' z  ', ' .z ', ' y  ', ' .y ' /

      IAND (IZV,IZW) = AND (IZV, IZW)


      IF (NQNEWH.NE.0)  CALL DPHEAD
      IFLBL  = 0
      IFLAFT = 0
      JSL = IQ(LDO+1)
      NSL = IQ(LDO+2)
      IF (NSL.LE.0)                RETURN
      IF (MODE.EQ.1)               GO TO 54

      IFLDEL = IAND (IQ(LDO),KM5+KM6)
      IFLCUT = IFLDEL

      LNACT = LACT
      IF (LNACT.EQ.0)              GO TO 38
      LNORG = IQ(LDO-2)
      LINUM = IQ(LDO+3)
      LEVEL = JBYT (IQ(LDO),21,5)
      CALL DPTAG (0,LEVEL,0)
      GO TO 39

   38 LLACT = 0
      LLORG = 0
      IF (JSL.LT.LQLEND(2))  THEN
          LINUM = JSL - JSLORG
        ELSE
          LINUM = -9999
        ENDIF
   39 JUMP  = MODE + 1
      GO TO (44, 54, 61, 41, 51, 71, 80), JUMP
C-      mode= 0   1   2   3   4   5   6

      CALL P_CRASH (' DPLIST - invalid MODE')
C----         Self material

   41 IFLBL = 1
      MASKF = '  + '
      MASKL = '  - '
      IF (JCCTYP.NE.MCCFAU)        GO TO 81
      MASKF= '!!!!'
      IFLCUT = 7
      GO TO 81

   44 MASKL = '  - '
      IF (JCCTYP.EQ.0)             GO TO 84
      MASKF = ' -- '
      IF (JCCTYP.EQ.MCCEND) THEN
          IFLAFT = IAND (IQ(LDO),KM17)
        ELSE
          IFLBL = IAND (IQ(LDO),KM17)
        ENDIF
      GO TO 81

C----         CDE expansion

   51 MASKF = '  : '
      GO TO 81

   54 IFLDEL = 0
      IFLCUT = 0
      LINUM = -9999
      MASKL = '    '
      IF (IFLSTA.EQ.0)             GO TO 84
      J = MIN (IFLSTA,2)
      IF (IFALTN.NE.0)  J= J+2
      MASKF = MASKK(J)
      GO TO 81

C----         Comment

   61 MASKF = '  c '
      GO TO 81

C----         Foreign material out from this deck

   71 MASKL = '  > '
      IF (IFLSTA.EQ.0)             GO TO 84

      IFLBL = 1
      IF (JCCTYP.EQ.MCCKEE)        GO TO 80

      IF (IAND(IQ(LDO),KM18).NE.0) GO TO 80
      MASKF = ' ++ '
      GO TO 81

C----         Stand-alone control line

   80 MASKF = '  + '
      IF (JCCTYP.GE.MCCDEC)  IFLBL= 1

C----     list the control-line first

   81 IF (NVEXDK(6).EQ.0)          GO TO 82
      IF (LNACT.NE.0)   MASKF(1:1) = '<'
      IF (IFLDEL.NE.0)  MASKF(3:3) = '('
      IF (IFLBL.NE.0)  THEN
          IF (NQUSED.GE.NQLTOK) THEN
              CALL DPPAGE
            ELSE
              CALL DPBLAN (1)
            ENDIF
        ENDIF

      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      CALL DPLINE (LINUM, MASKF, NTX, TEXT(JTX))

C--       print line with env. variable substituition

      IF (ICCSUB.NE.0)  CALL DPLINE (-1, '    ', NCHCCT, CCKORG)
      ICCSUB = 0

      IF (IFLAFT.NE.0)  CALL DPBLAN (-1)
   82 JSL = JSL + 1
      IF (IFLCUT.NE.0)             RETURN
      NSL = NSL - 1
      IF (NSL.LE.0)                RETURN
      LINUM = LINUM + 1

C----     list the body of the lines

   84 IF (LNACT.NE.0)   MASKL(1:1) = '<'
      IF (IFLDEL.NE.0)  MASKL(2:2) = '('
      DO 87  JJ=1,NSL
      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      CALL DPLINE (LINUM, MASKL, NTX, TEXT(JTX))
      LINUM = LINUM + 1
   87 JSL = JSL + 1
      RETURN
      END
