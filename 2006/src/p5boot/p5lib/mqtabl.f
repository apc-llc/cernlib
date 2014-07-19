CDECK  ID>, MQTABL.
      SUBROUTINE MQTABL

C-    Garbage collector - relocation table building
C-    only banks in division 1 can be collected
C.    started  8-oct-91

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      EQUIVALENCE (LS,IQLST)
      PARAMETER    (IQTVAL=0)

      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

C----              Decide where to put the table

      CALL MQGETT
      LIMIT = LQTE - 8
      LQTB  = LQTA + 3
      LQTE  = LQTA

      NWFREE = 0
      IQPART = 0
      MODE   = IQTVAL
      LQ(LQTA) = LQCSTA(1)

      LN    = LQCSTA(1)
      LSTOP = LQCEND(1)
      GO TO 24

C------            Next bank

   21 MODE = NEW
   22 LN   = IQLNX
   24 IF (LN.GE.LSTOP)             GO TO 39
      CALL QBLOW (LN)
      NEW = ISHFTR (IQ(LN),30)
      IF (NEW.EQ.MODE)             GO TO 22
      IF (NEW.EQ.IQTVAL)           GO TO 36

C--       start of a dead group
      LQ(LQTE+1) = LN
      LQTE = LQTE + 3
      GO TO 21

C--       start of a live group
   36 LQ(LQTE) = LN
      IF (LQTE.LT.LIMIT)           GO TO 21
      MODE   = IQTVAL
      IQPART = 1

C------            End of table,
C--                compute relocation constants

   39 IF (MODE.EQ.IQTVAL)  THEN
          LQ(LQTE+1) = LQCEND(1)
          LQTE = LQTE + 3
        ENDIF
      LQ(LQTE) = LQCEND(1)

      DO 44 JTB=LQTB,LQTE,3
      NWFREE = NWFREE + LQ(JTB) - LQ(JTB-2)
   44 LQ(JTB+2) = -NWFREE

      LQ(LQTA+2) = 3
      NQRESV(1) = NQRESV(1) + NWFREE
      NQFREE(1) = NWFREE
      LQMTB = LQTB
      LQMTE = LQTE
      RETURN
      END
