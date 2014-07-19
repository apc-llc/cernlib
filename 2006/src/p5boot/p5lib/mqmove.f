CDECK  ID>, MQMOVE.
      SUBROUTINE MQMOVE

C-    Garbage collector - memory move
C.    started  8-oct-91

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      PARAMETER    (NSTEP=3)


C----              MOVE LOW BANKS

      IF (LQMTB.EQ.LQMTE)  THEN
          LQCEND(1) = LQ(LQMTE-2)
          RETURN
        ENDIF

      LQCEND(1) = LQ(LQMTE-2) + LQ(LQMTE-1)
      JTB  = LQMTB
      JEND = LQMTE

   41 LOLD = LQ(JTB)
      LNEW = LQ(JTB+2) + LOLD
      N    = LQ(JTB+1) - LOLD
      CALL UCOPY (LQ(LOLD),LQ(LNEW),N)
      JTB = JTB + NSTEP
      IF (JTB.NE.JEND)  GO TO 41
      RETURN
      END
