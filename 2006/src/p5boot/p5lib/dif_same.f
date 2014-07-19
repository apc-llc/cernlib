CDECK  ID>, DIF_SAME.
      SUBROUTINE DIF_SAME (NTHX,NTHY,NMAT)

C-    Compare the 2 lines NTHX and NTHY,
C-    return:  NMAT = 0/1 for no/yes

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
        PARAMETER   (LGLIV=4, LGLIV2=8)
      COMMON /DIFXC/ JX_ORG,  JY_ORG,  NX_NLI, NY_NLI
     +,              JX_FEND, JY_FEND, NLI_MAT
     +,              JX_NTH, JX_JSL, JX_JTX, JX_NCH
     +,              JY_NTH, JY_JSL, JY_JTX, JY_NCH
     +,                 LXV_FAI(LGLIV), LYV_FAI(LGLIV)
     +,                 LXV_SCA(LGLIV), LYV_SCA(LGLIV)
     +,                 LXV_MAT(LGLIV), LYV_MAT(LGLIV)
C--------------    End CDE                        -------------------
      LOGICAL LN_EQU


      IF (JX_NTH.NE.NTHX)  CALL DIF_LX (NTHX)
      IF (JY_NTH.NE.NTHY)  CALL DIF_LY (NTHY)

      N  = JX_NCH
      IF (JY_NCH.EQ.N)  THEN
          IF (N.EQ.0)              GO TO 24
          IF (LN_EQU(TEXT(JX_JTX),TEXT(JY_JTX),N))  GO TO 24
        ENDIF
      NMAT = 0
      RETURN

   24 NMAT = 1
      RETURN
      END
