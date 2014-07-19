CDECK  ID>, MQLMOV.
      SUBROUTINE MQLMOV

C-    Garbage collector - memory move, line slot numbers + text
C.    started  8-oct-91

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------
      PARAMETER    (NSTEP=3)


      IF (LQTB.EQ.LQTE)  THEN
          LQLEND(1) = LQ(LQTE-2)
          RETURN
        ENDIF

      JSLTO = LQ(LQTB-2)
      JTXTO = MLIAD(JSLTO)
      JTB   = LQTB

   41 JSLFR = LQ(JTB)
      NLI   = LQ(JTB+1) - JSLFR
      IF (JSLTO.NE.JSLFR+LQ(JTB+2))  CALL P_CRASH ('MQLMOV mismatch')

      JTXFR = MLIAD(JSLFR)
      NTX   = MLIAD(JSLFR+NLI) - JTXFR

      NTXRE = JTXTO - JTXFR

      DO 44  J=1,NLI
   44 MLIAD(JSLTO+J) = MLIAD(JSLFR+J) + NTXRE

      CALL CCOPYL (TEXT(JTXFR),TEXT(JTXTO),NTX)
      JTXTO = JTXTO + NTX
      JSLTO = JSLTO + NLI

      JTB = JTB + NSTEP
      IF (JTB.NE.LQTE)             GO TO 41

      NTFREE = MLIAD(LQLEND(1)) - JTXTO
      LQLEND(1) = JSLTO
      NQRESV(3) = NQRESV(3) + NTFREE
      NQFREE(3) = NTFREE
      RETURN
      END
