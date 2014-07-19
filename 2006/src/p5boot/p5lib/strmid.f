CDECK  ID>, STRMID.
      SUBROUTINE STRMID (JASM,JDIV,LINE,JPUT)

C-    Construct stream ID like FORT:0 into LINE(JPUT:)
C.    started 13-nov-93

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*128


      JP = JPUT
      LINE(JP:JP+15) = CH_TYP(JASM)
      JP = LNBLNK (LINE(1:JP+15)) + 1
      LINE(JP:JP) = ':'
      CALL CSETDI (JDIV, LINE,JP+1,JP+1)
      NESLAT = JP + 2
      RETURN
      END
