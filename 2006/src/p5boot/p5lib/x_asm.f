CDECK  ID>, X_ASM.
      SUBROUTINE X_ASM

C-    Analyse the current control line +ASM, ...
C.    started 23-jan-92

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
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
      CHARACTER    CCOL(256)*1,   MSG*56,       SLCOL(512)*1
      EQUIVALENCE (CCOL,CCKARD), (MSG,SLERRM), (SLCOL,SLLINE)

      PARAMETER (KXBY=KM1, KXAT=KM2, KXSP=KM3, KXMO=KM4, KXAL=KM5,
     +           KXBI=KM6, KXUS=KM7, KXEX=KM8, KXSU=KM9, KXRH=KM10,
     +           KXPR=KM11, KXLO=KM12, KXRE=KM13)

      PARAMETER   (NPOSS = 13)
      CHARACTER    POSS(NPOSS)*8

      DATA POSS   / 'BY*PASS '
     +,             'A*TTACH '
     +,             'SPL*IT  '
     +,             'MOD*IFY '
     +,             'ALI*AS  '
     +,             'BIN*D   '
     +,             'USE*D   '
     +,             'EXT*EN* '
     +,             'CCH*SUBS'
     +,             'RH*EADER'
     +,             'PRE*FIX '
     +,             'LOG     '
     +,             'REA*SSIG' /

C- for    +ASM, types, T=BYPASS
C-        +ASM, type,  T=REASSIGN, S=type
C-        +ASM, types, T=ATTACH                      .fname
C-        +ASM, types, T=SPLIT
C-        +ASM, types, T=SPLIT, PREFIX               .work/new_
C-        +ASM, types, T=SPLIT, LOG                  .fname
C-        +ASM, types, T=SPLIT, PREFIX, LOG          .work/fname
C-        +ASM, types, T=MODIF
C-        +ASM, types, T=MODIF, PREFIX               .work/new_
C-        +ASM, types, T=MODIF, LOG                  .fname
C-        +ASM, types, T=MODIF, PREFIX, LOG          .work/fname
C-        +ASM, types, T=ALIAS
C-        +ASM, types, T=BIND
C-        +ASM, types, T=USED
C-        +ASM, types, T=EXTENSION                   .tex
C-        +ASM, types, T=CCHSUB                      .&+!+
C-        +ASM, types, T=RHEAD,                      .text

      IAND (IZV,IZW) = AND (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)


      JBK_TYP = 0
      MASKOP = KXBI
      IF (NCCPT.EQ.0)              GO TO 21
      CALL CCOPT (POSS,NPOSS)
      IF (JCCBAD.NE.0)             GO TO 91
      MASKOP = MCCPAR(JCCPT+2)
      IF (MASKOP.EQ.0)             GO TO 92

C----         handle T=ALIAS

      IF (IAND(MASKOP,KXAL).NE.0)  THEN
          IF (IAND(MASKOP,NOT(KXAL)).NE.0)   GO TO 92
          CALL ASMALIA
          RETURN
        ENDIF

C--       ready all needed ASMH and ASML banks

   21 CALL ASMALL
      IF (JCCBAD.NE.0)             GO TO 91

C-    this routine analyses the NCCPD parameters at JCCPD
C-    it creates the needed ASMT & ASML banks
C-    it replaces:
C-                 MCCPAR(JCCPD+1) = JASM
C-                             +2) = JSTRM = 0 for type:0
C-                                           1     type:1
C-                                           2     type:2
C-      same for JCCPP                       3     type:3
C-                                           4     type:4
      IF (JBK_TYP.EQ.0)  THEN
          IF (NCCPT.EQ.0)          GO TO 92
        ENDIF
C----         handle T=REASSIGN, S=type

      IF (IAND(MASKOP,KXRE).EQ.0)        GO TO 24
      IF (IAND(MASKOP,NOT(KXRE)).NE.0)   GO TO 92
      IF (NCCPP.NE.1)                    GO TO 91
      IF (NCCPD.NE.1)                    GO TO 91

      JASMTG = MCCPAR(JCCPP+1)
      JSTRTG = MCCPAR(JCCPP+2)
      JASM   = MCCPAR(JCCPD+1)
      JSTR   = MCCPAR(JCCPD+2)

      LASMT = LQ(LHASM-JASM)
      IF (JSTR.EQ.0)  THEN
          JA = 1
          JE = 4
        ELSE
          JA = JSTR
          JE = JSTR
        ENDIF

      DO 23  JL=JA,JE
      JS = JSTRTG
      IF (JS.EQ.0)  JS= JL
      LASML = LQ(LASMT-JL-1)
   23 IQ(LASML+11) = 8*JASMTG + JS
      RETURN

C--                parameter S= illegal except for T=REASS

   24 IF (NCCPP.NE.0)                    GO TO 91

C----         handle T=BYPASS

      IF (IAND(MASKOP,KXBY).EQ.0)        GO TO 27
      IF (IAND(MASKOP,NOT(KXBY)).NE.0)   GO TO 92

      DO 26  J=1,NCCPD
      JASM = MCCPAR(JCCPD+1)
      JSTR = MCCPAR(JCCPD+2)
      LASML = LQ(LHASM-JASM)
      IF (JSTR.NE.0)  LASML= LQ(LASML-JSTR-1)
      IQ(LASML+1) = 0
      IQ(LASML+2) = 0
   26 JCCPD = JCCPD + 3
      RETURN

C----         handle T=USED

   27 IF (IAND(MASKOP,KXUS).EQ.0)        GO TO 30
      IF (IAND(MASKOP,NOT(KXUS)).NE.0)   GO TO 92
      RETURN


C------       Do all but T=ALIAS, T=REASS, T=BY, T=USED

C--       ready the free-field parameter in the comment field
   30 CALL CCCOMG (1)

   31 JASMF = MCCPAR(JCCPD+1)
      JSTRF = MCCPAR(JCCPD+2)
      LASMT = LQ(LHASM-JASMF)
      LASML = LASMT
      IF (JSTRF.NE.0)  LASML= LQ(LASML-JSTRF-1)

C----         handle T=ATTACH

      IF (IAND(MASKOP,KXAT).EQ.0)        GO TO 34
      IF (IAND(MASKOP,NOT(KXAT)).NE.0)   GO TO 92

      IF (NCHCCC.EQ.0)             GO TO 93

      CHLIEX = ' '
      IXEXT = IQ(LASML+5)
      IF (IXEXT.LT.0)  IXEXT= IQ(LASMT+5)
      IF (IXEXT.GT.0)  CALL NA_GET (IXEXT,CHLIEX,1)
      CALL CCCRAK (6)

C--       unlink the output file if it exists already

      IQ(LASML+1) = 0
      IQ(LASML+2) = 1
      IQ(LASML+3) = IXCCC
      CALL FLINK (LUN_TYP+1, -1, IXCCC, 0)

      IF (NCCPD.GE.2)              GO TO 42
      RETURN

C----         handle T=SPLIT and T=MODIFY

   34 IF (IAND(MASKOP,KXSP).EQ.0)                 GO TO 36
      IF (IAND(MASKOP,NOT(KXSP+KXPR+KXLO)).NE.0)  GO TO 92
      JMODE = 2
      GO TO 37

   36 IF (IAND(MASKOP,KXMO).EQ.0)                 GO TO 41
      IF (IAND(MASKOP,NOT(KXMO+KXPR+KXLO)).NE.0)  GO TO 92
      JMODE = 3

   37 IQ(LASML+1) = 0
      IQ(LASML+2) = JMODE

      IF (NCCPD.GE.2)              GO TO 42
      GO TO 49

C----         handle T=BIND

   41 IF (IAND(MASKOP,KXPR+KXLO).NE.0)    GO TO 51
      IF (IAND(MASKOP,KXBI).EQ.0)         GO TO 61
      IF (IAND(MASKOP,NOT(KXBI)).NE.0)    GO TO 92

   42 MBIND = 8*JASMF + JSTRF

      JC = JCCPD + 3
      NC = NCCPD - 1
      IF (NC.LE.0)                 RETURN

      DO 47  J=1,NC
      JASM = MCCPAR(JC+1)
      JSTR = MCCPAR(JC+2)
      LASM = LQ(LHASM-JASM)
      IF (JSTR.NE.0)  LASM= LQ(LASM-JSTR-1)
      IF (IQ(LASM+1).EQ.0)         GO TO 97
      IQ(LASM+1) = MBIND
   47 JC = JC + 3
   49 IF (IAND(MASKOP,KXPR+KXLO).NE.0)  GO TO 52
      RETURN

C----         handle T=PREFIX, LOG

   51 IF (IAND(MASKOP,NOT(KXPR+KXLO)).NE.0)  GO TO 92
   52 MODE = IAND (ISHFTR(MASKOP,10), 3)
      IF (IQ(LASML+2).LT.2)        GO TO 94
      IF (NCHCCC.EQ.0)             GO TO 53
      CHLIEX = ' '
      CALL CCCRAK (6)
      GO TO 54

C--       no file name on c/line, use command line parameter

   53 IF (JSTRF.GE.3)              GO TO 95
      IF (JASMF.GT.JDA_TYP)        GO TO 95

C-            nypatchy pam fort read print cc as data  f:2 c:2 a:2 d:2
C-            JP=        1    2    3     4  5  6    7    8   9  10  11
C-            JASMF=          1             2  3    4    1   2   3   4

      IF (JSTRF.LT.2)  THEN
          JP = 2
          IF (JASMF.GE.2)  JP= JASMF + 3
        ELSE
          JP = JASMF + 7
        ENDIF
      IXCCC = IXLUN(JP)
      IF (IXCCC.EQ.0)              GO TO 95
      CALL NA_GET (IXCCC,CCCOMF,1)
      NCHCCC = NDSLAT

C--       change the extension to be .log

      JD = ICFILA ('/', CCCOMF,1,NCHCCC)
      JD = NGSLAT + 1
      JD = ICFILA ('.', CCCOMF,JD,NCHCCC)
      CCCOMF(JD:JD+3) = '.log'
      NCHCCC = JD + 3
      IXCCC = NA_LONG (CCCOMF(1:NCHCCC))

C--       for T=PREFIX (not LOG)

   54 IF (MODE.GE.2)               GO TO 57
      IQ(LASML+4) = IXCCC
      RETURN

C--       for T=LOG

   57 IQ(LASML+3) = IXCCC
      CALL FLINK (LUN_TYP+1, -1, IXCCC, 0)
      IF (MODE.EQ.2)               RETURN

C-        for T=PREFIX, LOG

      NPRE = ICFILA ('/',CCCOMF,1,NCHCCC)
      IF (NPRE.GE.NCHCCC)          GO TO 96

      IQ(LASML+4) = NA_LONG (CCCOMF(1:NPRE))
      RETURN

C----         handle T=EXTENSION

   61 IF (IAND(MASKOP,KXEX).EQ.0)        GO TO 63
      IF (IAND(MASKOP,NOT(KXEX)).NE.0)   GO TO 92

      CALL CCCOIX (IQ(LASML+5))
      GO TO 69

C----         handle T=CCHSUB

   63 IF (IAND(MASKOP,KXSU).EQ.0)        GO TO 64
      IF (IAND(MASKOP,NOT(KXSU)).NE.0)   GO TO 92

      CALL CCCOIX (IQ(LASML+7))
      GO TO 69

C----         handle T=RHEAD

   64 IF (IAND(MASKOP,KXRH).EQ.0)   GO TO 92

      CALL CCCOIX (IQ(LASML+6))

   69 JCCPD = JCCPD + 3
      NCCPD = NCCPD - 1
      IF (NCCPD.NE.0)              GO TO 31
      RETURN

C------            Trouble

   91 MSG = 'Syntax error'
      GO TO 98
   92 MSG = 'Illegal set of T= parameters'
      GO TO 98
   93 MSG = 'Missing file name'
      GO TO 98
   94 MSG = 'Only for streams in SPLIT or MODIFY mode'
      GO TO 98
   95 MSG = 'The trailing free-field parameter is missing'
      GO TO 98
   96 MSG = 'Illegal file name for T=PREFIX,LOG'
      GO TO 98
   97 MSG = 'Trying to bind a stream which is already physical'

   98 N = LNBLNK (MSG)
      CALL FAILCC (1, MSG(1:N))
      RETURN
      END
