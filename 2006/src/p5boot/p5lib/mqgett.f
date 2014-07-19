CDECK  ID>, MQGETT.
      SUBROUTINE MQGETT

C-    Garbage collector - find biggest space for the relocation table
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


C--       control division 2 empty

      LQTA = LQCEND(1)
      IF (LQCSTA(2).EQ.LQCEND(2))  THEN
          NWAV = LQCSTA(3) - LQTA
          GO TO 21
        ENDIF

C--       control division  1/2

      NWAV = LQCSTA(2) - LQTA

C--       control division  2/3

      IF (NWAV.LT.LQCSTA(3)-LQCEND(2))  THEN
          LQTA = LQCEND(2)
          NWAV = LQCSTA(3) - LQTA
        ENDIF

C--       text division 2/3

   21 IF (NWAV+8.LT.LQLSTA(4)-LQLEND(2))  THEN
          LQTA = LQLEND(2) + 2
          NWAV = LQLSTA(4) - LQTA - 2
          LQTA = LQTA + NQOFFS
        ENDIF

      LQTE = LQTA + NWAV
      RETURN
      END
