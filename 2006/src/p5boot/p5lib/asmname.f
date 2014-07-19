CDECK  ID>, ASMNAME.
      SUBROUTINE ASMNAME (NNAME,NAME)

C-    Decompose NAME = type:0 into components ACT_TYP and NDV_TYP

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
C--------------    End CDE              --------------------------------
      CHARACTER    NAME*32
      DIMENSION    NTRAN(4)
      DATA  NTRAN  / JFO_TYP, JAS_TYP, JDA_TYP, JCC_TYP /

C--   Translate:  21 FORT:1   22 AS:1   23 DATA:1   24 CC:1
C--               31 FORT:2   32 AS:2   33 DATA:2   34 CC:2

      NXY = NNAME
      JOLD = ICDECI (NAME,1,NXY)
      IF (NGSLAT.NE.0)             GO TO 29
      IF (JOLD.LT.21)              GO TO 91
      JSTR = 1
      IF (JOLD.GE.31)  THEN
          JSTR = 2
          JOLD = JOLD - 10
        ENDIF
      JOLD = JOLD - 20
      IF (JOLD.GT.4)               GO TO 91
      JASM = NTRAN(JOLD)
      ACT_TYP = CH_TYP(JASM)
      NDV_TYP = JSTR
      JBK_TYP = JBK_TYP + 1
      RETURN

   29 CONTINUE
      NXX = ICFIND(':', NAME,1,NXY) - 1
      IF (NXX.EQ.0)                GO TO 91
      ACT_TYP = NAME(1:NXX)

C--       check old format R=!

      IF (NXX.EQ.1)  THEN
          IF (ICTYPE(ACT_TYP).LT.3)   GO TO 91
        ENDIF

      IF (NXX.EQ.NXY)  THEN
          JSTR = 0
        ELSE
          JSTR = ICDECI (NAME, NXX+2,NXY)
          IF (JSTR.GT.4)           GO TO 91
          IF (NDSLAT.EQ.0)         GO TO 91
          IF (NGSLAT.NE.0)         GO TO 91
        ENDIF

      NDV_TYP = JSTR
      RETURN

C--                Fault

   91 CALL FAILCC (1,'Faulty logical stream name')
      NDV_TYP = -1
      RETURN
      END
