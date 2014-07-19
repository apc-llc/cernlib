CDECK  ID>, DIF_BACK.
      SUBROUTINE DIF_BACK (NPOSSP,NMATP)

C-    Compare backwards the NPOSS > 0 lines from the current
C-    On entry:  line vectors positioned to the first line to be checked
C-    On return:  NMAT > 0 the number of matching lines found,
C-                         line vectors positioned to the first
C-                         line of the match
C-                NMAT = 0 no match, line vectors unchanged

        PARAMETER   (LGLIV=4, LGLIV2=8)
      COMMON /DIFXC/ JX_ORG,  JY_ORG,  NX_NLI, NY_NLI
     +,              JX_FEND, JY_FEND, NLI_MAT
     +,              JX_NTH, JX_JSL, JX_JTX, JX_NCH
     +,              JY_NTH, JY_JSL, JY_JTX, JY_NCH
     +,                 LXV_FAI(LGLIV), LYV_FAI(LGLIV)
     +,                 LXV_SCA(LGLIV), LYV_SCA(LGLIV)
     +,                 LXV_MAT(LGLIV), LYV_MAT(LGLIV)
C--------------    End CDE                        -------------------


      NPOSS = NPOSSP
      NMAT  = 0
      CALL DIF_SAME (JX_NTH, JY_NTH, IFSAME)
      IF (IFSAME.EQ.0)             GO TO 28

C--           Current lines match

   24 NMAT  = NMAT  + 1
      NPOSS = NPOSS - 1
      IF (NPOSS.EQ.0)              GO TO 28

C--           How about the previous?

      CALL DIF_SAME (JX_NTH-1, JY_NTH-1, IFSAME)
      IF (IFSAME.NE.0)             GO TO 24

C--           Not equal, position to start of match

      CALL DIF_NX
      CALL DIF_NY
   28 NMATP = NMAT
      RETURN
      END
