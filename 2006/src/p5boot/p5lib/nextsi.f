CDECK  ID>, NEXTSI.
      SUBROUTINE NEXTSI (JSLA,JTYPEF,NHOW)

C-    Check single line at JSLA is a header +DECK, etc.
C-    NHOW =  0 scanning for PAM, look at first 3 keys
C-           >0 scanning for cradle, look at all keys, ignore +TITle
C-              =3:  blank deck of cradle, +MORE allowed

C-    return JTYPEF 1,2,3(,4,5) if found
C-    return JTYPEF -13 if line +MORE in the blank deck of the cradle
C-    return JTYPEF zero if not found

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ),  MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      PARAMETER   (NPOSSA=6)
      CHARACTER    POSS(NPOSSA)*4
      DATA POSS    / '+DEC', '+PAT', '+TIT', '+PAM', '+QUI', '+MOR' /


      IF (NHOW.EQ.0)  THEN
          NPOSX = 3
        ELSEIF (NHOW.EQ.3)  THEN
          NPOSX = 6
        ELSE
          NPOSX = 5
        ENDIF

      JSL   = JSLA
      JTGO  = MLIAD(JSL)
      IF (TEXT(JTGO).NE.'+')       GO TO 98

C--       next possibility

      JPOSS = 0
   21 JPOSS = JPOSS + 1
      IF (JPOSS.GT.NPOSX)          GO TO 98
      JTX = JTGO + 1
      JC  = 2
      IVP = ICHAR(POSS(JPOSS)(2:2))
   22 IVT = ICHAR(TEXT(JTX))
      IF (IVT.NE.IVP)  THEN
          IF (IVT-IVP.NE.32)       GO TO 21
        ENDIF
      IF (JC.EQ.4)                 GO TO 28
      JTX = JTX + 1
      JC  = JC + 1
      IVP = ICHAR(POSS(JPOSS)(JC:JC))
      GO TO 22

C----              return

   28 IF (NPOSX.GE.5)              GO TO 29
      JTYPEF = JPOSS
      RETURN

C--       cradle

   29 IF (JPOSS.EQ.3)              GO TO 98
      IF (JPOSS.EQ.6)  JPOSS= -13
      JTYPEF = JPOSS
      RETURN

   98 JTYPEF = 0
      RETURN
      END
