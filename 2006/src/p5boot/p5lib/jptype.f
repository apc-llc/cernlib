CDECK  ID>, JPTYPE.
      FUNCTION JPTYPE (LINE)

C-    Find control-card type of line in LINE(1:N)
C-    without looking at the control-character in LINE(1:1)

      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
C--------------    End CDE              --------------------------------
      CHARACTER    LINE(64)*1, CHWK*4, CHWK1*1, CHWK2*1, CHWK3*1
      EQUIVALENCE (CHWK1,CHWK(1:1)),(CHWK2,CHWK(2:2)),(CHWK3,CHWK(3:3))

      PARAMETER   (NPOSSA = 39)
      CHARACTER    POSS(NPOSSA)*4
      DIMENSION    MTYPE(NPOSSA)

      DATA  POSS  / '+CDE' , '+SEQ' , '+SEL' , '+NIL' , '+INC'
     +,             '+IF,' , '+IFN' , '+ELS' , '+END'
     +,             '+KEE' , '+REP' , '+ADD' , '+ADB' , '+DEL'
     +,             '+DEC' , '+PAT' , '+TIT' , '+EOD' , '+SKI'
     +,             '+USE' , '+EXE' , '+LIS' , '+DIV' , '+XDI' , '+IMI'
     +,             '+OPT' , '+PAR' , '+SHO'
     +,             '+ASM' , '+ONL' , '+FOR' , '+SUP'
     +,             '+NAM' , '+GAP' , '+UPD' , '+MOR'
     +,             '+PAM' , '+QUI' , '+KIL'  /

      DATA  MTYPE / MCCCDE , MCCSEQ , MCCSEL , MCCNIL , MCCINC
     +,             MCCTRU , MCCFAL , MCCELS , MCCEND
     +,             MCCKEE , MCCREP , MCCADD , MCCADB , MCCDEL
     +,             MCCDEC , MCCPAT , MCCTIT , MCCEOD , MCCSKI
     +,             MCCUSE , MCCEXE , MCCLIS , MCCDIV , MCCXDI , MCCIMI
     +,             MCCOPT , MCCOP2 , MCCSHO
     +,             MCCASM , MCCONL , MCCFOR , MCCSUS
     +,             MCCNAM , MCCGAP , MCCUPD , MCCMOR
     +,             MCCPAM , MCCQUI , MCCKIL  /


      IF (LINE(2).EQ.'_')          GO TO 61

C--       scan normal possibilities

      JPOSS = 0
   22 JPOSS = JPOSS + 1
      IF (JPOSS.GT.NPOSSA)         GO TO 29
      JC  = 2
      IVP = ICHAR(POSS(JPOSS)(2:2))
      IVT = ICHAR(LINE(2))
   24 IF (IVT.NE.IVP)  THEN
          IF (IVT-IVP.NE.32)       GO TO 22
        ENDIF
      IF (JC.EQ.4)                 GO TO 41
      JC  = JC + 1
      IVP = ICHAR(POSS(JPOSS)(JC:JC))
      IVT = ICHAR(LINE(JC))
      GO TO 24

C--       not a Patchy key

   29 JTYPE  = 0
      GO TO 42

C----              return

   41 JTYPE = MTYPE(JPOSS)
   42 JPTYPE = JTYPE
      RETURN

C--         handle  +___IF, IFNOT, ELSE, ENDIF

   61 JF = 2
   62 JF = JF + 1
      IF (LINE(JF).EQ.'_')  GO TO 62

      CHWK(1:1) = LINE(JF)
      CHWK(2:2) = LINE(JF+1)
      CHWK(3:3) = LINE(JF+2)
      IF (CHWK1.EQ.'I')            GO TO 71
      IF (CHWK1.EQ.'i')            GO TO 71
      IF (CHWK1.NE.'E')  THEN
          IF (CHWK1.NE.'e')        GO TO 29
        ENDIF

C--       ELSE or ENDIF

      IF (CHWK2.EQ.'L')            GO TO 64
      IF (CHWK2.EQ.'l')            GO TO 64
      IF (CHWK2.NE.'N')  THEN
          IF (CHWK2.NE.'n')        GO TO 29
        ENDIF
      IF (CHWK3.NE.'D')  THEN
          IF (CHWK3.NE.'d')        GO TO 29
        ENDIF
      JTYPE = MCCEND
      GO TO 42

C--       ELSE

   64 IF (CHWK3.NE.'S')  THEN
          IF (CHWK3.NE.'s')        GO TO 29
        ENDIF
      JTYPE = MCCELS
      GO TO 42

C--       IF or IFNOT

   71 IF (CHWK2.NE.'F')  THEN
          IF (CHWK2.NE.'f')        GO TO 29
        ENDIF
      IF (CHWK3.NE.'N')  THEN
          IF (CHWK3.NE.'n')        GO TO 74
        ENDIF
      JTYPE = MCCFAL
      GO TO 42

C--       IF,

   74 JF = JF + 1
   75 JF = JF + 1
      IF (LINE(JF).EQ.' ')  GO TO 75
      IF (LINE(JF).NE.',')         GO TO 29
      JTYPE = MCCTRU
      GO TO 42
      END
