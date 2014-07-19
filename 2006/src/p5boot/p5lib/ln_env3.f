CDECK  ID>, LN_ENV3.
      FUNCTION LN_ENV3 (JSL)

C-    Copy the line at slot JSL to the end of division 3 with
C-    substitution of environment variables
C.    started 22-dec-93

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
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
          CALL P_CRASH ('LN_ENV3: LQLSTA(3) not = LQLEND(2)')
        ENDIF

      JSLFR = JSL
      JTXFR = MLIAD(JSLFR)
      NTXFR = MLIAD(JSLFR+1) - JTXFR - NCHNEWL

      JSLTO = LQLEND(3)
      JTXTO = MLIAD(JSLTO)

      JSLLIM = LQLSTA(4)
      JTXLIM = MLIAD(JSLLIM)
      NSLAV  = JSLLIM - JSLTO - 2
      NTXAV  = JTXLIM - JTXTO - 400

      IF (NSLAV.LT.2)
     +        CALL NOSPACE ('no text store left in text division 3')
      IF (NTXAV.LT.NTXFR)
     +        CALL NOSPACE ('no text store left in text division 3')

      CALL CENVIR (TEXT(JTXFR),NTXFR, TEXT(JTXTO),1,NTXAV,0)

      IF (NFSLAT.GE.4)
     +        CALL NOSPACE ('no text store left in text division 3')
      IF (NFSLAT.GE.2)             GO TO 91
      IF (NGSLAT.EQ.0)             GO TO 41
      JTXE = JTXTO + NDSLAT
      TEXT(JTXE) = CHAR(NEWLN)
      JTXE = JTXE + 1
      MLIAD(JSLTO+1) = JTXE

      LQLEND(3) = JSLTO + 1
      LN_ENV3 = JSLTO
      NGSLAT  = 1
      RETURN

C--           no substitution, return NGSLAT = 0

   41 NDSLAT  = NTXFR
      LN_ENV3 = JSLFR
      RETURN

C--           syntax problems

   91 LN_ENV3 = -1
      RETURN
      END
