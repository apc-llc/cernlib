CDECK  ID>, MQGARB.
      SUBROUTINE MQGARB

C-    Garbage collector - control routine
C.    started  8-oct-91

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      PARAMETER    (KQGARB=1)



      CALL VZERO (NQRESV,3)

      IF (LQGARB.EQ.0)             GO TO 41
   24 CALL MQKILL
      IF (LQGARB.EQ.0)             GO TO 27

C----              Collect Text garbage

      CALL QSORTI (1,KQGARB)
      CALL MQLTAB
      CALL MQLREL
      CALL MQLMOV

C----              Collect Control garbage

   27 CALL MQTABL
      IF (NQFREE(1).EQ.0)          GO TO 41
      CALL MQRELC
      CALL MQMOVE
      IF (LQGARB.NE.0)             GO TO 24
      IF (IQPART.NE.0)             GO TO 27

C----              Everbody happy?

   41 IF (NQRESV(1).LT.0)          GO TO 71
      IF (NQRESV(2).LT.0)          GO TO 71
      IF (NQRESV(3).LT.0)          GO TO 71
      RETURN

C---               Not enough space

   71 IQUEST(1) = 99
      IQUEST(2) = NQRESV(1)
      IQUEST(3) = NQRESV(2)
      IQUEST(4) = NQRESV(3)
      CALL P_KILL ('MQGARB, no space.')
      END
