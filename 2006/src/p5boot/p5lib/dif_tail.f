CDECK  ID>, DIF_TAIL.
      SUBROUTINE DIF_TAIL (MGOOD)

C-    Check whether the very last lines of the 2 ranges match,
C-    on entry:    MGOOD with the last 2 entries of the GOOD table
C-    on exit:     MGOOD updated

        PARAMETER   (LGLIV=4, LGLIV2=8)
      COMMON /DIFXC/ JX_ORG,  JY_ORG,  NX_NLI, NY_NLI
     +,              JX_FEND, JY_FEND, NLI_MAT
     +,              JX_NTH, JX_JSL, JX_JTX, JX_NCH
     +,              JY_NTH, JY_JSL, JY_JTX, JY_NCH
     +,                 LXV_FAI(LGLIV), LYV_FAI(LGLIV)
     +,                 LXV_SCA(LGLIV), LYV_SCA(LGLIV)
     +,                 LXV_MAT(LGLIV), LYV_MAT(LGLIV)
C--------------    End CDE                        -------------------
      DIMENSION    MGOOD(6)


      IF (MGOOD(4).NE.0)           RETURN
      NMAT  = MGOOD(1)
      NLDEL = MGOOD(5) - (MGOOD(2) + NMAT)
      NLINS = MGOOD(6) - (MGOOD(3) + NMAT)
      NPOSS = MIN (NLDEL,NLINS)
      IF (NPOSS.EQ.0)              RETURN

      CALL DIF_LX (NX_NLI)
      CALL DIF_LY (NY_NLI)
      CALL DIF_BACK (NPOSS,NMAT)
      IF (NMAT.EQ.0)               RETURN
      MGOOD(4) = NMAT
      MGOOD(5) = JX_NTH
      MGOOD(6) = JY_NTH
      RETURN
      END
