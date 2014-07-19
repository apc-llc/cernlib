CDECK  ID>, ASMFIND.
      SUBROUTINE ASMFIND (DTYPE,JASM)

C-    Find the ASM stream for data type DTYPE,
C-    return its number in JASMF, or =0 if it does not exist;

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
C--------------    End CDE              --------------------------------
      CHARACTER    DTYPE*(*)


      NXX  = MIN (LEN(DTYPE), 16)
      NXX  = LNBLNK (DTYPE(1:NXX))

C--       check alias

      JAL = MAX_TYP - NAL_TYP
      J   = ICNTH (DTYPE(1:NXX), CH_TYP(JAL+1), NAL_TYP)
      IF (J.NE.0)  THEN
          J = J + JAL
          NDSLAT = J
          JASM   = JU_TYP(J)
          RETURN
        ENDIF

      NDSLAT = 0
      JASM   = ICNTH (DTYPE(1:NXX), CH_TYP, N_TYP)
      RETURN
      END
