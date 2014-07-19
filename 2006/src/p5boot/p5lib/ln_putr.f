CDECK  ID>, LN_PUTR.
      SUBROUTINE LN_PUTR (LINE,NCH,JSL)

C-    Transfer a line from local to the store,
C-    reverse at slot JSL-1

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ),  MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      CHARACTER*512  LINE

      JTX = MLIAD(JSL) - NCH - NCHNEWL
      MLIAD(JSL-1) = JTX

      IF (NCH.GT.0)  CALL CCOPYL (LINE,TEXT(JTX),NCH)
      JTX = JTX + NCH
      TEXT(JTX) = CHAR (NEWLN)
      RETURN
      END
