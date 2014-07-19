CDECK  ID>, NEXTCC.
      SUBROUTINE NEXTCC (CHCC,JSLA,JSLE,JSLF,JTYPEF)

C-    Find next Patchy c/c line in line-range JSLA to JSLE-1
C-    having control-character CHCC in column 1

C-    return type number JTYPEF and JSLF if found
C-    return JTYPEF=zero and JSLF=JSLE if not found

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ),  MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      CHARACTER    CHCC*1, CHCCU*1


      CHCCU = CHCC
      JSL   = JSLA -1

C--       next line

   11 JSL = JSL + 1
      IF (JSL.GE.JSLE)             GO TO 98
      JTX = MLIAD(JSL)
      IF (TEXT(JTX).NE.CHCCU)      GO TO 11
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      IF (NTX.LT.4)                GO TO 11

      JTYPE = JPTYPE (TEXT(JTX))
      IF (JTYPE.EQ.0)              GO TO 11

C----              return

      JTYPEF = JTYPE
      JSLF   = JSL
      RETURN

   98 JTYPEF = 0
      JSLF   = JSLE
      RETURN
      END
