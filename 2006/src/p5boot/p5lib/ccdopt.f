CDECK  ID>, CCDOPT.
      SUBROUTINE CCDOPT (LOOK, JTYPE)

C-    Handle T= parameters on current +PATCH or +DECK c/l
C-    on entry: LOOK = 0  do not create new data-types
C-                     1  normal operation
C-    return:  JTYPE = data type 1,2,3,...  FORT, CC, ASL, ...
C-                     zero if not specified
C-             MCCPAR(JCCPT+2): bit 1 if T=REPEAT
C-                                  2 if T=JOIN
C-             NDSLAT = zero if no error

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
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
C--------------    End CDE              --------------------------------

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      NFAI = 0
      JASM = 0
      IF (NCCPT.EQ.0)              GO TO 99
      MSK = 0
      LOC = JCCPT
      NTH = NCCPT
   24 LOC = LOC + 3
      JXX = MCCPAR(LOC+1)
      NXX = MCCPAR(LOC+2)
      NXX = MIN (NXX, 16)
      CALL ASMFIND (CCKARD(JXX:JXX+NXX-1), JF)

      J = JF
      IF (J.EQ.0)  THEN
          IF (LOOK.EQ.0)           GO TO 27
          CALL ASMCREA (CCKARD(JXX:JXX+NXX-1), J, 1)
        ENDIF

      IF (J.LT.0)  THEN
          MSK = IOR (MSK, -J)
          GO TO 27
        ENDIF

      IF (JASM.EQ.0)               GO TO 26

C--       accept:  T=DATA, INCL  (DATA for p4, INCL for p5 if declared)

      IF (JF  .EQ.0)         GO TO 25
      IF (JASM.GT.4)         GO TO 25
      IF (J   .GT.4)         GO TO 26
   25 NFAI = NFAI + 1
      GO TO 27

   26 JASM = J
   27 NTH = NTH - 1
      IF (NTH.GT.0)                GO TO 24

      MCCPAR(JCCPT+2) = MSK
   99 NDSLAT = NFAI
      JTYPE  = JASM
      RETURN
      END
