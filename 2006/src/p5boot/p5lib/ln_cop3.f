CDECK  ID>, LN_COP3.
      FUNCTION LN_COP3 (JSL,NLI)

C-    Transfer: Copy NLI lines starting at slot JSL into division 3
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

      IF (LQLSTA(3).NE.LQLEND(2)) THEN
          CALL P_CRASH ('LN_COP3: LQLSTA(3) not = LQLEND(2)')
        ENDIF

      JSLFR = JSL
      NSL   = NLI
      JTXFR = MLIAD(JSLFR)
      NTX   = MLIAD(JSLFR+NSL) - JTXFR

      JSLTO = LQLEND(3)
      JTXTO = MLIAD(JSLTO)
      NTXRE = JTXTO - JTXFR

      JSLLIM = LQLSTA(4)
      JTXLIM = MLIAD(JSLLIM)
      IF (JSLTO+NSL+4 .GE.JSLLIM)
     +        CALL NOSPACE ('no line slots left in text division 3')
      IF (JTXTO+NTX+8 .GE.JTXLIM)
     +        CALL NOSPACE ('no text store left in text division 3')

      DO 44  J=1,NSL
   44 MLIAD(JSLTO+J) = MLIAD(JSLFR+J) + NTXRE

      CALL CCOPYL (TEXT(JTXFR),TEXT(JTXTO),NTX)

      LQLEND(3) = JSLTO + NSL
      LN_COP3 = JSLTO
      RETURN
      END
