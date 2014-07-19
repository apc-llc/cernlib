CDECK  ID>, DIF_HEAD.
      SUBROUTINE DIF_HEAD

C-    Find first failure between the two sets
C-    return NDSLAT = 0  failure found, the line vectors are positioned
C-                                      to the start of the failure
C-                    1  end of one of the sets found, the line vectors
C-                              are positioned to the last matching line
C-                    2  the two sets are identical

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
      COMMON /SLATE/ NDSLAT,NESLAT,DUMMY(38)
      LOGICAL LN_EQU


      NDSLAT = 0
   21 IF (JX_NCH.NE.JY_NCH)        GO TO 29
      IF (JX_NCH.EQ.0)             GO TO 22
      IF (.NOT.LN_EQU(TEXT(JX_JTX), TEXT(JY_JTX), JX_NCH))  GO TO 29

   22 IF (JX_NTH.EQ.NX_NLI)        GO TO 27
      IF (JY_NTH.EQ.NY_NLI)        GO TO 28
      CALL DIF_NX
      CALL DIF_NY
      GO TO 21

   27 NDSLAT = 1
      IF (JY_NTH.NE.NY_NLI)        GO TO 29
   28 NDSLAT = NDSLAT + 1
   29 RETURN
      END
