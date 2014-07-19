CDECK  ID>, ASMCREA.
      SUBROUTINE ASMCREA (DTYPE,JASMF,IFRUN)

C-    Lift the ASMT and ASML banks for data type DTYPE
C-        IFRUN = 0: create with default connections for +ASM
C-                1: unkown data type seen when running,
C                    create with connections for DATA

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
C--------------    End CDE              --------------------------------
      CHARACTER    DTYPE*(*)


      IF (N_TYP+NAL_TYP.EQ.MAX_TYP)    GO TO 91
      IF (N_TYP.EQ.63)                 GO TO 91

      N_TYP = N_TYP + 1
      JASM  = N_TYP
      CH_TYP(JASM) = DTYPE

      CALL MQLIFT (LASMT, LHASM,-JASM, JBKSMT,3)

      IF (IFLAUX.NE.0)             GO TO 39
      IQ(LASMT+1) = -1
      LDATA = LQ(LHASM-4)

C--       create the ASML banks

      DO 37  JDIV=1,4
      CALL MQLIFT (LASML, LASMT,-JDIV-1, JBKSML,3)
      IF (IFRUN.EQ.0)  THEN
          IQ(LASML+1) = -1
          IQ(LASML+5) = -1
          IQ(LASML+6) = -1
          IQ(LASML+7) = -1
        ELSE
          LASMLD = LQ(LDATA-JDIV-1)
          MUSE  = IQ(LASMLD+1)
          IF (MUSE.EQ.0)  MUSE = 32 + JDIV
          IQ(LASML+1) = MUSE
          IQ(LASML+5) = IQ(LASMLD+5)
        ENDIF
   37 CONTINUE
   39 JASMF = N_TYP
      IF (IFRUN.EQ.0)              RETURN
      IF (IFLAUX.NE.0)             RETURN
      SLERRM = 'Undeclared data-type: ' // DTYPE
      N = LENOCC (SLERRM)
      CALL FAILMSG (0,1, 0,SLERRM(1:N))
      RETURN

C--                Fault, capacity exeeded

   91 IF (IFLAUX.NE.0)             GO TO 39
      IF (IFRUN.EQ.0)  CALL P_KILL ('Too many ASM data types')
      JASMF = 4
      RETURN
      END
