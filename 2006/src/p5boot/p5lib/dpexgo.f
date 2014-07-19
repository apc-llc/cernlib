CDECK  ID>, DPEXGO.
      SUBROUTINE DPEXGO

C-    Ready to write first line of new routine,
C-    called from DPEXE.
C.    started 21-jan-92

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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
      COMMON /DEPMOD/MO_JSA, MO_JSL, MO_JSE
      CHARACTER*8             CH_SUBS, CH_SUBT
      COMMON /DEPSUB/NCH_SUB, CH_SUBS, CH_SUBT
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------
      EQUIVALENCE (LASML,JD_SML), (LASMP,JD_SMP), (LXASM,JD_SMX)

      CHARACTER    CHMARK*1, LINE*256, COLS(256)*1
      EQUIVALENCE (LINE,SLLINE)
      EQUIVALENCE (COLS,LINE)


      IF (JD_NEW.LT.0)             GO TO 29

C--           Decide logical stream at the start of deck

      JD_DTD = JD_DTN
      IF (JD_DTD.EQ.0)  JD_DTD= JD_DTP

      JD_DIV = NVEXDK(3) + 2*NVEXDK(4) + 1
C-        =  1 normal, 2 diverted, 3 xdiv/normal, 4 xdiv/diverted

C--           Find the ASML bank for the logical stream

      LASMT = LQ(LHASM-JD_DTD)
      LASML = LQ(LASMT-JD_DIV-1)

C--           do the re-assignment if requested

      J = IQ(LASML+11)
      IF (J.NE.0)  THEN
          JD_DTD = J/8
          J      = MOD (J,8)
          IF (J.NE.0)  JD_DIV= J
          LASMT = LQ(LHASM-JD_DTD)
          LASML = LQ(LASMT-JD_DIV-1)
        ENDIF

C--           Find the ASML bank for the physical stream

      MUSE = IQ(LASML+1)
      IF (MUSE.EQ.0)  THEN
          LASMP  = LASML
          JDP_DT = JD_DTD
          JDP_DV = JD_DIV
          GO TO 24
        ENDIF

      JDP_DT = MUSE / 8
      JDP_DV = MOD (MUSE,8)
      LASM = LQ(LHASM-JDP_DT)
      IF (JDP_DV.NE.0)  THEN
          LASMP = LQ(LASM-JDP_DV-1)
        ELSE
          LASMP = LASM
        ENDIF

   24 LXASM = LQ(LASMP-1)

C----         Decide the processing mode

      IQ(LASML+8) = IQ(LASML+8) + 1
      JD_SPL = IQ(LASMP+2)
      JD_MOD = 0

C--           exit for T=BYPASS

      IF (JD_SPL.EQ.0)  THEN
          NVEXDK(1) = 0
          NVEXDK(5) = NVEXDK(2)
          IQ(LXASM+3) = IQ(LXASM+3) + 1
          RETURN
        ENDIF

      IX_EXT = IQ(LASML+5)
      IX_RH  = IQ(LASML+6)
      IX_SUB = IQ(LASML+7)

      IF (IX_SUB.EQ.0)             GO TO 29

C--       prepare the control-character substitution ready for use

      CALL NA_GET (IX_SUB,LINE,1)
      NCH_SUB = MIN (8, NDSLAT/2)

      JJ = 0
      DO 27 J=1,NCH_SUB
      CH_SUBS(J:J) = COLS(JJ+1)
      CH_SUBT(J:J) = COLS(JJ+2)
   27 JJ = JJ + 2

C----         All mode parameters ready, start

   29 JD_NEW = 0
      JD_NLI = 0
      JD_LUN = IQ(LXASM+2)
      JD_MOD = JD_SPL
      IF (NEWINC.NE.0)  JD_MOD= 0
      IF (JD_MOD.LT.3)  JD_MOD= 0
      IF (JD_SPL.GE.2)             GO TO 41

C------       Normal sequential mode            ---------------

      IF (JD_LUN.GT.0)             GO TO 34

C--           open output file for first deck on this stream

      LUN_TYP = LUN_TYP + 1
      JD_LUN  = LUN_TYP
      IXFLUN  = IQ(LASMP+3)

      IF (IXFLUN.EQ.0)
     +      CALL P_FATAL ('output file name missing')

      CALL FLINK (JD_LUN, 6, -1, 0)
      IF (IXFLUN.LT.0)  CALL P_FATAL ('OPEN failure')
      JD_LUN = LUNFD
      IQ(LXASM+2) = JD_LUN

C--       Write routine header card if needed

   34 IF (IX_RH.EQ.0)              RETURN

      JTX = NA_JTX (IX_RH)
      NTX = NDSLAT

      CHMARK = TEXT(JTX)

      JTX  = JTX + 1
      NTX  = NTX - 1
      NPUT = 0

   37 JF = ICFILA (CHMARK,TEXT(JTX),1,NTX)
      N  = JF - 1
      IF (N.GT.0)  THEN
          CALL CCOPYL (TEXT(JTX),COLS(NPUT+1),N)
          NPUT = NPUT + N
          JTX  = JTX  + N
          NTX  = NTX  - N
        ENDIF

      IF (NTX.GT.0)  THEN
          CALL NA_GET (IXEXID,LINE,NPUT+1)
          NPUT = NESLAT - 1
          JTX  = JTX  + 1
          NTX  = NTX  - 1
          IF (NTX.GT.0)            GO TO 37
        ENDIF

      NPUT = NPUT + 1
      LINE(NPUT:NPUT) = CHAR(NEWLN)
      CALL CIPUT (JD_LUN,LINE,NPUT,ISTAT)
      IF (ISTAT.NE.0)  CALL P_FATAM ('CIO write fails')
      JD_NLI = 1
      RETURN

C------       SPLIT mode                        ---------------

   41 IF (JD_LUN.GE.0)             GO TO 44
      IQ(LXASM+2) = 0
      IXFLUN = IQ(LASMP+3)
      IF (IXFLUN.EQ.0)             GO TO 44

C--           open the log file for first deck on this stream

      LUN_TYP = LUN_TYP + 1
      JD_LUN  = LUN_TYP
      CALL FLINK (JD_LUN, 6, -1, 0)
      IF (IXFLUN.LT.0)  CALL P_FATAL ('OPEN failure')
      JD_LUN = LUNFD
      IQ(LXASM+2) = JD_LUN

C--           construct the file name for the split file

   44 JD_LOG = JD_LUN
      JD_LUN = IQ(LHASM+1)
      CHLIFI = ' '
      JP = 1

      IX = IQ(LASMP+4)
      IF (IX.NE.0)  THEN
          CALL NA_GET (IX, CHLIFI, JP)
          JP = NESLAT
        ENDIF

      CALL NA_GET (IXEXID, CHLIFI, JP)
      CALL CUTOL  (CHLIFI(JP:NESLAT))
      JP = NESLAT

      IF (IX_EXT.NE.0)  THEN
          CALL NA_GET (IX_EXT, CHLIFI, JP)
          JP = NESLAT
        ENDIF
      NLIFI = JP - 1

      MODEFI = 6
      IF (JD_MOD.NE.0)  MODEFI= 7

      CALL FLINK (JD_LUN, MODEFI, 0, 0)
      IF (IXFLUN.LT.0)  CALL P_FATAL ('OPEN failure')
      IF (LUNOLD.EQ.0)  JD_MOD = 0
      JD_LUN = LUNFD
      IF (JD_MOD.EQ.0)             RETURN

C------       Initiate processing for MODIFY    ---------------

      MO_JSA = LQLEND(3)
      MO_JSL = MO_JSA
      MO_JSE = LQLSTA(4)

      JTXA = MLIAD(MO_JSA)
      JTXE = MLIAD(MO_JSE)
      NMAX = JTXE - JTXA - 2048
      IF (LUNSIZ.GE.NMAX)          GO TO 79

      CALL CIGET (JD_LUN, TEXT(JTXA), NMAX, NTXR, ISTAT)
      IF (ISTAT.NE.0)              GO TO 79

      JTXE  = JTXA + NTXR
      JSLE  = MO_JSL
      LIMIT = MO_JSE - 4

      DO 54  JTX=JTXA,JTXE-1
      IF (ICHAR(TEXT(JTX)).EQ.NEWLN)  THEN
          JSLE = JSLE + 1
          MLIAD(JSLE) = JTX + 1
          IF (JSLE.GE.LIMIT)       GO TO 79
        ENDIF
   54 CONTINUE

      IF (MLIAD(JSLE).NE.JTXE)  THEN
          TEXT(JTXE) = CHAR(NEWLN)
          JTXE = JTXE + 1
          JSLE = JSLE + 1
          MLIAD(JSLE) = JTXE
        ENDIF

      MO_JSE    = JSLE
      LQLEND(3) = JSLE
      RETURN

   79 JD_MOD = 0
      RETURN
      END
