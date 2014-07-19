CDECK  ID>, ASMALIA.
      SUBROUTINE ASMALIA

C-    Analyse +ASM, D=typ1, typ2, ..., T=ALIAS

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
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
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*60
      EQUIVALENCE (MSG,SLERRM)


      LOC = JCCPD
      NTH = NCCPD
      IF (NTH.LT.2)                RETURN

C--       get the aliasing target

      JTX = NA_JTX (MCCPAR(LOC+1))
      NN  = NDSLAT
      CALL ASMNAME (NN, TEXT(JTX))
      IF (NDV_TYP.LT.0)            RETURN
      CALL ASMFIND (ACT_TYP, JUSE)
      IF (JUSE.EQ.0)  CALL ASMCREA (ACT_TYP, JUSE, 0)

C--       get the alias names

   24 LOC = LOC + 3
      NTH = NTH - 1
      IF (NTH.EQ.0)                RETURN
      JTX = NA_JTX (MCCPAR(LOC+1))
      NN  = NDSLAT
      CALL ASMNAME (NN, TEXT(JTX))
      IF (NDV_TYP.LT.0)            GO TO 24
      CALL ASMFIND (ACT_TYP, JASM)
      IF (JASM.NE.0)               GO TO 31

C--       new name

      IF (N_TYP+NAL_TYP.EQ.MAX_TYP)   GO TO 91
      JAL = MAX_TYP - NAL_TYP
      CH_TYP(JAL) = ACT_TYP
      JU_TYP(JAL) = JUSE
      NAL_TYP     = NAL_TYP + 1
      GO TO 24

C----         Alias name already known

   31 JAL = NDSLAT
      IF (JAL.EQ.0)                GO TO 92
      IF (JU_TYP(JAL).EQ.JUSE)     GO TO 24
      GO TO 93

C----         Errors

   91 CALL FAILCC (2,'Data-type catalog full')

   92 MSG = 'Stream ' // ACT_TYP // 'already established'
      GO TO 96

   93 MSG = 'Trying to redefine alias ' // ACT_TYP
   96 CALL CSQMBL (MSG,1,50)
      N = NDSLAT
      CALL FAILCC (1,MSG(1:N))
      END
