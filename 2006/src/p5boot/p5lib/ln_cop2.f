CDECK  ID>, LN_COP2.
      FUNCTION LN_COP2 (JSL,NLI)

C-    Transfer: Copy NLI lines starting at slot JSL to division 2
C.    started 27-feb-92

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


      JSLFR = JSL
      NSL   = NLI
      JTXFR = MLIAD(JSLFR)
      NTX   = MLIAD(JSLFR+NSL) - JTXFR

      JSLTO = LQLEND(2)
      JTXTO = MLIAD(JSLTO)
      NTXRE = JTXTO - JTXFR

      JSLLIM = LQLSTA(4)
      JTXLIM = MLIAD(JSLLIM)
      IF (JSLTO+NSL+4 .GE.JSLLIM)
     +        CALL NOSPACE ('no line slots left in text division 2')
      IF (JTXTO+NTX+8 .GE.JTXLIM)
     +        CALL NOSPACE ('no text store left in text division 2')

      DO 44  J=1,NSL
   44 MLIAD(JSLTO+J) = MLIAD(JSLFR+J) + NTXRE

      CALL CCOPYL (TEXT(JTXFR),TEXT(JTXTO),NTX)

      LQLEND(2) = JSLTO + NSL
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      LN_COP2 = JSLTO
      RETURN
      END
