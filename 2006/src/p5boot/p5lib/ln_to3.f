CDECK  ID>, LN_TO3.
      FUNCTION LN_TO3 (LINE)

C-    Transfer: Set a line from local to division 3
C.    started 22-jun-93

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------
      CHARACTER      LINE*(*)

      IF (LQLSTA(3).NE.LQLEND(2)) THEN
          CALL P_CRASH ('LN_TO3: LQLSTA(3) not = LQLEND(2)')
        ENDIF

      NCH   = LNBLNK (LINE)
      JSLTO = LQLEND(3)
      JTXTO = MLIAD(JSLTO)
      JTXE  = JTXTO + NCH

      JSLLIM = LQLSTA(4)
      JTXLIM = MLIAD(JSLLIM)
      IF (JSLTO+4 .GE.JSLLIM)
     +        CALL NOSPACE ('no line slots left in text division 3')
      IF (JTXE+8  .GE.JTXLIM)
     +        CALL NOSPACE ('no text store left in text division 3')

      IF (NCH.GT.0)  CALL CCOPYL (LINE,TEXT(JTXTO),NCH)
      TEXT(JTXE) = CHAR (NEWLN)
      JTXE = JTXE + 1
      MLIAD(JSLTO+1) = JTXE
      LQLEND(3) = JSLTO + 1
      LN_TO3 = JSLTO
      RETURN
      END
