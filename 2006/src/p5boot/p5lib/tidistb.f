CDECK  ID>, TIDISTB.
      SUBROUTINE TIDISTB

C-    Discard trailing blanks on the lines in memory
C.    started 26-may-94

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------
      EQUIVALENCE (NCOUNT,IQUEST(1))

      IAND (IZV,IZW) = AND (IZV, IZW)

      NCOUNT = 0
      IF (IAND(IQ(LQHOLD),1) .EQ.0)    RETURN
      CALL SBIT0 (IQ(LQHOLD),1)

      JSLSTA = IQ(LQHOLD+1)
      JSLEND = IQ(LQHOLD+1) + IQ(LQHOLD+2)

C----         scan to find next line with trailing blank

      JSL1   = JSLSTA
      JTXPUT = MLIAD(JSL1)
   41 JTXTAK = MLIAD(JSL1)
      JSL2   = JSLEND - 1

      DO 42  J=JSL1,JSL2
      JTXF = MLIAD(J)
      JTXN = MLIAD(J+1) - NCHNEWL
      IF (JTXN.EQ.JTXF)            GO TO 42
      IF (TEXT(JTXN-1).EQ.' ')     GO TO 44
   42 CONTINUE
      IF (NCOUNT.EQ.0)             RETURN
      NCHCOP = MLIAD(JSLEND) - JTXTAK
      GO TO 51

C--           step to last non-blank on the line

   44 JSL2 = J
   45 JTXN = JTXN - 1
      IF (JTXN.GT.JTXF)  THEN
          IF (TEXT(JTXN-1).EQ.' ') GO TO 45
        ENDIF

      TEXT(JTXN) = CHAR(NEWLN)
      JTXN = JTXN + 1
      NCHCOP = JTXN - JTXTAK
      IF (NCOUNT.EQ.0)             GO TO 57

C--           copy the text and update the line slots

   51 CALL CCOPYL (TEXT(JTXTAK), TEXT(JTXPUT), NCHCOP)
      NSH = JTXPUT - JTXTAK

      DO 54  J=JSL1,JSL2
   54 MLIAD(J) = MLIAD(J) + NSH

   57 JTXPUT = JTXPUT + NCHCOP
      NCOUNT = NCOUNT + 1
      JSL1   = JSL2 + 1
      IF (JSL1.LT.JSLEND)          GO TO 41
      MLIAD(JSLEND) = JTXPUT
      RETURN
      END
