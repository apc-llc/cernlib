CDECK  ID>, MQRELC.
      SUBROUTINE MQRELC

C-    Garbage collector - Relocator
C-    only links in banks in divisions 1 and 3 need relocation
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
      JDIV  = 1
      GO TO 15

C--------------    Trigger control

C--       What next ?
   12 IF (LN.EQ.0)                 GO TO 17
      IF (JDIV.EQ.3)               RETURN
      JDIV  = 3
      LN    = LQCSTA(JDIV)
      LDEAD = LQCEND(JDIV)
      LSTOP = LDEAD
      IF (LN.EQ.LSTOP)             GO TO 12
      IF (NENTR)             53,43,23

C--       Trigger structural permanent links
   15 L1   = 0
      L2   = 0
      LN   = 0
      LSTOP= 0
      LS   = NQSYSS + 1
      GO TO 19

C--       Trigger working space links and division 1
   17 LS   = NQLINK + 1
      L2   = LS
      LMTB = LQMTB-3
      LN   = LQ(LMTB)
      LDEAD= LQ(LMTB+1)
      LSTOP= LQCEND(1)
   19 IF (NENTR)             54,44,24

C--------------    2 or more relocation intervals       -------------

C------            Next bank,  check if dead group

   21 IF (LN.EQ.LSTOP)             GO TO 12
      IF (LN.NE.LDEAD)             GO TO 23
      LMTB = LMTB + 3
      LN   = LQ(LMTB)
      LDEAD= LQ(LMTB+1)
      GO TO 21

C--                Next bank,  alive

   23 CALL QBLOW (LN)
      L2 = LS - IQNS
      L1 = LN
      LN = IQLNX

C----              Next link

   24 L1 = L1 + 1
      IF (L1.EQ.LS)                GO TO 21
   25 LINK  = LQ(L1)
      IF (LINK.GE.LFIX)            GO TO 24
      IF (LINK.LT.LKEEP)           GO TO 24

      IF (LINK.LT.LQ(LQTB))        GO TO 31
      JLOW = 0
      JHI  = NENTR + 1

C--                Binary search in relocator table

   27 JEX = (JHI+JLOW) / 2
      IF (JEX.EQ.JLOW)             GO TO 29
      IF (LINK.GE.LQ(LQTB+3*JEX))  GO TO 28
      JHI  = JEX
      GO TO 27

   28 JLOW = JEX
      GO TO 27

   29 JTB = LQTB + 3*JLOW
      IF (LINK.GE.LQ(JTB+1))       GO TO 31
      LQ(L1) = LINK + LQ(JTB+2)
      GO TO 24

C--       Bridging for dead, structural link
   31 IF (L1.LT.L2)                GO TO 38
      LQ(L1)= LQ(LINK-1)
      GO TO 25

   38 LQ(L1)= 0
      GO TO 24

C--------------    1 relocation interval only           -------------

C------            Next bank,  check if dead group

   41 IF (LN.EQ.LSTOP)             GO TO 12
      IF (LN.NE.LDEAD)             GO TO 43
      LMTB = LMTB + 3
      LN   = LQ(LMTB)
      LDEAD= LQ(LMTB+1)
      GO TO 41

C--                Next bank,  alive

   43 CALL QBLOW (LN)
      L2 = LS - IQNS
      L1 = LN
      LN = IQLNX

C----              Next link

   44 L1 = L1 + 1
      IF (L1.EQ.LS)                GO TO 41
   45 LINK  = LQ(L1)
      IF (LINK.GE.LFIX)            GO TO 44
      IF (LINK.LT.LKEEP)           GO TO 44

      IF (LINK.LT.LQ(LQTB))        GO TO 47
      IF (LINK.GE.LQ(LQTB+1))      GO TO 47
      LQ(L1) = LINK + LQ(LQTB+2)
      GO TO 44

C--       Bridging for dead, structural link
   47 IF (L1.LT.L2)                GO TO 48
      LQ(L1)= LQ(LINK-1)
      GO TO 45

   48 LQ(L1)= 0
      GO TO 44

C--------------    No relocation interval               ----------------

C------            Next bank,  check if dead group

   51 IF (LN.EQ.LSTOP)             GO TO 12
      IF (LN.NE.LDEAD)             GO TO 53
      LMTB = LMTB + 3
      LN   = LQ(LMTB)
      LDEAD= LQ(LMTB+1)
      GO TO 51

C--                Next bank,  alive

   53 CALL QBLOW (LN)
      L2 = LS - IQNS
      L1 = LN
      LN = IQLNX

C----              Next link

   54 L1 = L1 + 1
      IF (L1.EQ.LS)                GO TO 51
   55 LINK= LQ(L1)
      IF (LINK.GE.LFIX)            GO TO 54
      IF (LINK.LT.LKEEP)           GO TO 54
      IF (L1.LT.L2)                GO TO 58

C--       Bridging for dead, structural link
   57 LINK  = LQ(LINK-1)
      IF (LINK.GE.LFIX)            GO TO 59
      IF (LINK.LT.LKEEP)           GO TO 59
      GO TO 57

   58 LQ(L1)= 0
      GO TO 54

   59 LQ(L1)= LINK
      GO TO 54
      END
