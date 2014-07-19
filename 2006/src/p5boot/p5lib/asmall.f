CDECK  ID>, ASMALL.
      SUBROUTINE ASMALL

C-    For c/l +ASM, D=typ1, typ2, ... analyse the parameters at JCCPD,
C-    create the needed ASMT & ASML banks,
C-    replace:
C-             MCCPAR(JCCPD+1) = JASM
C-                         +2) = JSTR   = 0 for type
C-                                    1 2 3     type:0 :1 :2

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

C--           do the DT= parameters

      IFL = 0
      LOC = JCCPD
      NTH = NCCPD
   24 JTX = NA_JTX (MCCPAR(LOC+1))
      NN  = NDSLAT
      CALL ASMNAME (NN, TEXT(JTX))
      IF (NDV_TYP.LT.0)            GO TO 91

      CALL ASMFIND (ACT_TYP, JASM)
      IFAL = NDSLAT
      IF (JASM.EQ.0)  THEN
          CALL ASMCREA (ACT_TYP, JASM, 0)
        ENDIF

      MCCPAR(LOC)   = IFAL
      MCCPAR(LOC+1) = JASM
      MCCPAR(LOC+2) = NDV_TYP
      LOC = LOC + 3
      NTH = NTH - 1
      IF (NTH.GT.0)                GO TO 24
      IF (IFL.NE.0)                RETURN

C--           do also the S= parameter

      NTH = NCCPP
      IF (NTH.EQ.0)                RETURN
      LOC = JCCPP
      IFL = 7
      GO TO 24

C----         Fault

   91 JCCBAD = 1
      RETURN
      END
