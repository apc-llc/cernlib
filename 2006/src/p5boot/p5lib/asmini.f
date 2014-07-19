CDECK  ID>, ASMINI.
      SUBROUTINE ASMINI

C-    Lift the initial ASM structure
C.    started 23-jan-92

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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

      CHARACTER*(*)  CHRHF, CHRHC, CHRHI, CHRHA
      PARAMETER    (CHRHF= ':CDECK  ID>, :. ')
      PARAMETER    (CHRHC= ':/*DECK ID>, :. */')
      PARAMETER    (CHRHI= ':/*DECK ID>, :.h */')
      PARAMETER    (CHRHA= ':|DECK  ID>, :.')

      CHARACTER*(*)  CHEXTF, CHEXTC, CHEXTI, CHEXTA, CHEXTD, CHEXTS
      PARAMETER    (CHEXTF= '.f')
      PARAMETER    (CHEXTC= '.c')
      PARAMETER    (CHEXTI= '.h')
      PARAMETER    (CHEXTA= '.s')
      PARAMETER    (CHEXTD= ' ')
      PARAMETER    (CHEXTS= '.sh')


      N_TYP   = 0
      NAL_TYP = 2

      JAL = MAX_TYP - NAL_TYP

      CH_TYP(JAL+1) = 'R*EPEAT'
      CH_TYP(JAL+2) = 'J*OIN'
      JU_TYP(JAL+1) = -1
      JU_TYP(JAL+2) = -2

C--       register routine header masks
      IXRHF = NA_LONG (CHRHF)
      IXRHC = NA_LONG (CHRHC)
      IXRHI = NA_LONG (CHRHI)
      IXRHA = NA_LONG (CHRHA)

C--       lift the support bank ASMH

      CALL MQLIFT (LHASM, 0,7, JBKSMH,3)

      IQ(LHASM+1) = 30
      LUN_TYP     = 30

C--       lift the ASM banks for "FORTRAN",  JASM = JFO_TYP = 1

      CALL ASMCREA ('FORT', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTF)
      IQ(LASMT+6) = IXRHF

      IF (IXLUN(2).NE.0)  THEN
          IQ(LASMT+1) = 0
          IQ(LASMT+2) = 1
          IQ(LASMT+3) = IXLUN(2)
          CALL FLINK (LUN_TYP, -1, IXLUN(2), 0)
        ENDIF

      IF (IXLUN(8).NE.0)  THEN
          LASML = LQ(LASMT-3)
          IQ(LASML+1) = 0
          IQ(LASML+2) = 1
          IQ(LASML+3) = IXLUN(8)
          CALL FLINK (LUN_TYP, -1, IXLUN(8), 0)
        ENDIF

C--       lift the ASM banks for "CC",  JASM = JCC_TYP = 2

      CALL ASMCREA ('CC', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTC)
      IQ(LASMT+6) = IXRHC

      IF (IXLUN(5).NE.0)  THEN
          IQ(LASMT+1) = 0
          IQ(LASMT+2) = 1
          IQ(LASMT+3) = IXLUN(5)
          CALL FLINK (LUN_TYP, -1, IXLUN(5), 0)
        ENDIF

      IF (IXLUN(9).NE.0)  THEN
          LASML = LQ(LASMT-3)
          IQ(LASML+1) = 0
          IQ(LASML+2) = 1
          IQ(LASML+3) = IXLUN(9)
          CALL FLINK (LUN_TYP, -1, IXLUN(9), 0)
        ENDIF

C--       lift the ASM banks for "AS",  JASM = JAS_TYP = 3

      CALL ASMCREA ('AS', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTA)
      IQ(LASMT+6) = IXRHA

      IF (IXLUN(6).NE.0)  THEN
          IQ(LASMT+1) = 0
          IQ(LASMT+2) = 1
          IQ(LASMT+3) = IXLUN(6)
          CALL FLINK (LUN_TYP, -1, IXLUN(6), 0)
        ENDIF

      IF (IXLUN(10).NE.0)  THEN
          LASML = LQ(LASMT-3)
          IQ(LASML+1) = 0
          IQ(LASML+2) = 1
          IQ(LASML+3) = IXLUN(10)
          CALL FLINK (LUN_TYP, -1, IXLUN(10), 0)
        ENDIF

C--       lift the ASM banks for "DATA",  JASM = JDA_TYP = 4

      CALL ASMCREA ('DATA', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTD)

      IF (IXLUN(7).NE.0)  THEN
          IQ(LASMT+1) = 0
          IQ(LASMT+2) = 1
          IQ(LASMT+3) = IXLUN(7)
          CALL FLINK (LUN_TYP, -1, IXLUN(7), 0)
        ENDIF

      IF (IXLUN(11).NE.0)  THEN
          LASML = LQ(LASMT-3)
          IQ(LASML+1) = 0
          IQ(LASML+2) = 1
          IQ(LASML+3) = IXLUN(11)
          CALL FLINK (LUN_TYP, -1, IXLUN(11), 0)
        ENDIF

C--       lift the ASM banks for "SHELL",  JASM = JSH_TYP = 5

      CALL ASMCREA ('SHELL', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTS)
      IQ(LASMT+7) = NA_LONG ('&+')

C--       lift the ASM banks for "CRAD",  JASM = JCR_TYP = 6

      CALL ASMCREA ('CRAD', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG ('.cra')
      IQ(LASMT+7) = NA_LONG ('&+')

C--       lift the ASM banks for "INCL",  JASM = JIN_TYP = 7

      CALL ASMCREA ('INCL', JASM, 0)
      LASMT = LQ(LHASM-JASM)

      IQ(LASMT+5) = NA_LONG (CHEXTI)
      IQ(LASMT+6) = IXRHI

C----         Create standard aliases

      JAL = JAL - 4
      CH_TYP(JAL+1) = 'CO*MPILE'
      CH_TYP(JAL+2) = 'X*CC'
      CH_TYP(JAL+3) = 'A*S*'
      CH_TYP(JAL+4) = 'D*ATA'

      JU_TYP(JAL+1) = JFO_TYP
      JU_TYP(JAL+2) = JCC_TYP
      JU_TYP(JAL+3) = JAS_TYP
      JU_TYP(JAL+4) = JDA_TYP

      NAL_TYP = MAX_TYP - JAL
      RETURN
      END
