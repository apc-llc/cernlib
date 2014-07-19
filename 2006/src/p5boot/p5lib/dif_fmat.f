CDECK  ID>, DIF_FMAT.
      SUBROUTINE DIF_FMAT (MILILE, MIMATP)

C-    Find next matching lines, at leat MIMAT+1 lines
C-    with a length of MILILE or more columns

C-    On input:   old/new JX_/JY_NTH positioned to the start of failure
C-                JX_FEND/Y  last possible failing line

C-    On output:  LXV_FAI/_YFAI param. of the start lines of failure
C-                LXV_MAT/_YMAT param. of the start lines of match
C-                NLI_MAT  the length of this match,
C-                       =0 : no match found

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


      CALL UCOPY (JX_NTH, LXV_FAI, LGLIV2)
      MILENGO = MILILE
      MIMAT   = MIMATP
      MILENCO = MIN (MILENGO,9)
      NLI_MAT = 0
      GO TO 16

C--                Step to first possible start lines

   15 IF (JX_NTH.GE.JX_FEND)       GO TO 81
      CALL DIF_NX
   16 IF (JX_NCH.LT.MILENGO)       GO TO 15
      GO TO 18

   17 IF (JY_NTH.GE.JY_FEND)       GO TO 81
      CALL DIF_NY
   18 IF (JY_NCH.LT.MILENGO)       GO TO 17

      CALL UCOPY (JX_NTH, LXV_SCA, LGLIV2)

      JHALTX = JX_FEND - MIMAT
      JHALTY = JY_FEND - MIMAT
      IF (JX_NTH.GT.JHALTX)        GO TO 81
      IF (JY_NTH.GT.JHALTY)        GO TO 81

      JENDY = JY_NTH
      GO TO 22

C----         Next X line, compare against Y lines so far

   21 IF (JENDX.EQ.JHALTX)         GO TO 31
      CALL DIF_NX
   22 JENDX = JX_NTH
      NCH   = JX_NCH
      IF (NCH.LT.MILENGO)          GO TO 31

      CALL UCOPY (LYV_SCA, JY_NTH, LGLIV)
      GO TO 25

   24 IF (JY_NTH.EQ.JENDY)         GO TO 31
      CALL DIF_NY

   25 IF (JY_NCH.NE.NCH)           GO TO 24
      IF (.NOT.LN_EQU(TEXT(JY_JTX),TEXT(JX_JTX),NCH))  GO TO 24
      GO TO 41

C----         Next Y line, compare against X lines so far

   31 IF     (JENDY.EQ.JHALTY)  THEN
          IF (JENDX.NE.JHALTX)     GO TO 21
          GO TO 81
        ENDIF
      CALL DIF_NY
      JENDY = JY_NTH
      NCH   = JY_NCH
      IF (NCH.LT.MILENGO)          GO TO 21

      CALL UCOPY (LXV_SCA, JX_NTH, LGLIV)
      GO TO 35

   34 IF (JX_NTH.EQ.JENDX)         GO TO 21
      CALL DIF_NX

   35 IF (JX_NCH.NE.NCH)           GO TO 34
      IF (.NOT.LN_EQU(TEXT(JX_JTX),TEXT(JY_JTX),NCH))  GO TO 34

C--       Found first line of possible match

   41 CALL UCOPY (JX_NTH, LXV_MAT, LGLIV2)
      NEXMAT = 0
      NSAME  = 0
   42 NSAME  = NSAME + 1
      IF (JX_NTH.EQ.JX_FEND)       GO TO 46
      IF (JY_NTH.EQ.JY_FEND)       GO TO 46
      CALL DIF_NX
      CALL DIF_NY
      N  = JX_NCH
      IF (JY_NCH.NE.N)             GO TO 46
      IF (N.EQ.0)                  GO TO 42
      IF (.NOT.LN_EQU(TEXT(JX_JTX),TEXT(JY_JTX),N))  GO TO 46
      IF (N.LT.MILENCO)            GO TO 42
      NEXMAT = NEXMAT + 1
      GO TO 42

C--       Is the match is long enough ?

   46 IF (NEXMAT.GE.MIMAT)         GO TO 51
      CALL DIF_LX (LXV_MAT(1) + NSAME -1)
      CALL DIF_LY (LYV_MAT(1) + NSAME -1)
      JENDX = MIN (MAX(JENDX,JX_NTH), JHALTX)
      JENDY = MIN (MAX(JENDY,JY_NTH), JHALTY)
      IF (JY_NTH.LT.JENDY)         GO TO 24
      IF (JX_NTH.LT.JENDX)         GO TO 34
      GO TO 21

C----              Match found

   51 NLI_MAT = NSAME

C--           Find identical short lines in front

      NPOSS = MIN (LXV_MAT(1)-LXV_FAI(1)
     +,            LYV_MAT(1)-LYV_FAI(1))
      IF (NPOSS.LE.0)              GO TO 87

      CALL UCOPY (LXV_MAT, JX_NTH, LGLIV2)
      CALL DIF_LX (JX_NTH-1)
      CALL DIF_LY (JY_NTH-1)
      CALL DIF_BACK (NPOSS,NPRE)
      IF (NPRE.NE.0)  CALL UCOPY (JX_NTH, LXV_MAT, LGLIV2)
      NLI_MAT = NLI_MAT + NPRE

C----              No match in the region

   81 CONTINUE
   87 RETURN
      END
