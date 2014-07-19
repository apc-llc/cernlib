CDECK  ID>, LN_TO4.
      FUNCTION LN_TO4 (LINE,NCH)

C-    Transfer: Set a line from local to the start of division 4
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
      CHARACTER*512  LINE

      NTX   = NCH + NCHNEWL
      JSLTO = LQLSTA(4)
      JTXTO = MLIAD(JSLTO) - NTX
      JSLTO = JSLTO - 1

      JSLLIM = LQLEND(3)
      JTXLIM = MLIAD(JSLLIM)
      IF (JSLTO.LE.JSLLIM+4)
     +        CALL NOSPACE ('no line slots left in text division 4')
      IF (JTXTO.LE.JTXLIM+8)
     +        CALL NOSPACE ('no text store left in text division 4')

      IF (NCH.GT.0)  CALL CCOPYL (LINE,TEXT(JTXTO),NCH)
      JTXE  = JTXTO + NTX
      TEXT(JTXE-1) = CHAR (NEWLN)
      MLIAD(JSLTO) = JTXTO
      LQLSTA(4) = JSLTO
      LN_TO4 = JSLTO
      RETURN
      END
