CDECK  ID>, MQLREL.
      SUBROUTINE MQLREL

C-    Garbage collector - Relocator, line slot numbers
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


      LKEEP = LQ(LQTB-2)
      LFIX  = LQ(LQTE)
      NENTR = (LQTE-LQTB) / 3
      JDIV  = 0

C----              Do next division

   12 IF (JDIV.EQ.1)               RETURN
      JDIV  = JDIV + 1
      LN    = LQCSTA(JDIV)
      LSTOP = LQCEND(JDIV)
      IF (NENTR)             51,41,21

C--------------    2 or more relocation intervals       -------------

C--                Next bank

   21 IF (LN.EQ.LSTOP)             GO TO 12
      CALL QBLOW (LN)
      LN = IQLNX
      IF (IQLI.EQ.0)               GO TO 21

      JSL  = IQ(LS+1)
      IF (JSL.GE.LFIX)             GO TO 21
      IF (JSL.LT.LKEEP)            GO TO 21

      IF (JSL.LT.LQ(LQTB))         GO TO 38
      JLOW = 0
      JHI  = NENTR + 1

C--                Binary search in relocator table

   27 JEX = (JHI+JLOW) / 2
      IF (JEX.EQ.JLOW)             GO TO 29
      IF (JSL.GE.LQ(LQTB+3*JEX))   GO TO 28
      JHI  = JEX
      GO TO 27

   28 JLOW = JEX
      GO TO 27

   29 JTB = LQTB + 3*JLOW
      IF (JSL.GE.LQ(JTB+1))        GO TO 38
      IQ(LS+1) = JSL + LQ(JTB+2)
      GO TO 21

   38 IQ(LS+1) = 0
      GO TO 21

C--------------    1 relocation interval only           -------------

C--                Next bank

   41 IF (LN.EQ.LSTOP)             GO TO 12
      CALL QBLOW (LN)
      LN = IQLNX
      IF (IQLI.EQ.0)               GO TO 41

      JSL  = IQ(LS+1)
      IF (JSL.GE.LFIX)             GO TO 41
      IF (JSL.LT.LKEEP)            GO TO 41

      IF (JSL.LT.LQ(LQTB))         GO TO 48
      IF (JSL.GE.LQ(LQTB+1))       GO TO 48
      IQ(LS+1) = JSL + LQ(LQTB+2)
      GO TO 41

   48 IQ(LS+1) = 0
      GO TO 41

C--------------    No relocation interval               ----------------

C--                Next bank

   51 IF (LN.EQ.LSTOP)             GO TO 12
      CALL QBLOW (LN)
      LN = IQLNX
      IF (IQLI.EQ.0)               GO TO 51

      JSL = IQ(LS+1)
      IF (JSL.GE.LFIX)             GO TO 51
      IF (JSL.LT.LKEEP)            GO TO 51
      IQ(LS+1) = 0
      GO TO 51
      END
