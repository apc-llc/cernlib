CDECK  ID>, NEXTXX.
      SUBROUTINE NEXTXX (KEY,JSLA,JSLE,JSLF)

C-    Find next line with key= +XXX in line-range JSLA to JSLE-1
C-    return JSLF if found, JSLF =zero if not found

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ),  MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      CHARACTER    KEY*4, UKEY*4


      UKEY = KEY
      JSL  = JSLA - 1

C--       next line

   11 JSL = JSL + 1
      IF (JSL.GE.JSLE)             GO TO 98
      JTX = MLIAD(JSL)
      IF (TEXT(JTX).NE.'+')        GO TO 11
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      IF (NTX.LT.4)                GO TO 11

      JTX = JTX + 1
      JKY = 2
      IVK = ICHAR(UKEY(2:2))
   22 IVT = ICHAR(TEXT(JTX))
      IF (IVT.NE.IVK)  THEN
          IF (IVT-IVK.NE.32)       GO TO 11
        ENDIF
      IF (JKY.EQ.4)                GO TO 29
      JTX = JTX + 1
      JKY = JKY + 1
      IVK = ICHAR(UKEY(JKY:JKY))
      GO TO 22

C----              return

   29 JSLF = JSL
      RETURN

   98 JSLF = 0
      RETURN
      END
