CDECK  ID>, LN_COP4.
      FUNCTION LN_COP4 (JSL,NLI)

C-    Transfer: Copy NLI lines starting at slot JSL into division 4
C.    started 19-dec-91

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

      JSLTO = LQLSTA(4)
      JTXTO = MLIAD(JSLTO) - NTX
      JSLTO = JSLTO - NSL
      NTXRE = JTXTO - JTXFR

      JSLLIM = LQLEND(3)
      JTXLIM = MLIAD(JSLLIM)
      IF (JSLTO.LE.JSLLIM+4)
     +        CALL NOSPACE ('no line slots left in text division 4')
      IF (JTXTO.LE.JTXLIM+8)
     +        CALL NOSPACE ('no text store left in text division 4')

      DO 44  J=0,NSL-1
   44 MLIAD(JSLTO+J) = MLIAD(JSLFR+J) + NTXRE

      CALL CCOPYL (TEXT(JTXFR),TEXT(JTXTO),NTX)

      LQLSTA(4) = JSLTO
      LN_COP4 = JSLTO
      RETURN
      END
