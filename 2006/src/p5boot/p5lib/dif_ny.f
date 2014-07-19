CDECK  ID>, DIF_NY.
      SUBROUTINE DIF_NY

C-    Step the current line on "new" by +1

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


      JY_NTH = JY_NTH + 1
      JY_JSL = JY_NTH + JY_ORG
      JY_JTX = MLIAD(JY_JSL)
      JY_NCH = MLIAD(JY_JSL+1) - JY_JTX - NCHNEWL
      RETURN
      END
