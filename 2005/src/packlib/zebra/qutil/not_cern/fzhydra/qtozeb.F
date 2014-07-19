*
* $Id: qtozeb.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: qtozeb.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE QTOZEB (VECT,LNSTA,MODE)









      COMMON /MQCF/  NQNAME,NQNAMD,NQNAMU, IQSTRU,NQSTRU, IQSYSB,NQSYSB     MQCF
      COMMON /MQCMOV/NQSYSS                                                 MQCM
      COMMON /MQCM/         NQSYSR,NQSYSL,NQLINK,LQWORG,LQWORK,LQTOL       +MQCM
     +,              LQSTA,LQEND,LQFIX,NQMAX, NQRESV,NQMEM,LQADR,LQADR2    +MQCM
      COMMON /MQCT/  IQTBIT,IQTVAL,LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH         MQCT
     +,              IQPART,NQFREE                                         +MQCT
      COMMON /QCN/   IQLS,IQID,IQNL,IQNS,IQND,IQFOUL                         QCN
      COMMON /QBITS/ IQDROP,IQMARK,IQCRIT,IQZIM,IQZIP,IQSYS                   SQ
                         DIMENSION    IQUEST(30)                             +SQ
                         DIMENSION                 LQ(99), IQ(99), Q(99)
          EQUIVALENCE (QUEST(1),IQUEST(1)), (LQUSER(1),LQ(1),IQ(1),Q(1))     +SQ
      COMMON /SQK/   KQMAIN,KQT,KQR,KQJ,KQF,KQZ,KQH(4),KQS(8)                +SQ
      COMMON //      QUEST(30),LQUSER(7),LQMAIN,LQT,LQR,LQJ,LQF,LQZ          +SQ
     +,              LQH(4),LQS(6),   LQWM,LQWF,LQWZ,LQWSYS(6),LQPRIV(7)     +SQ
     +,              LQ1,LQ2,LQ3,LQ4,LQ5,LQ6,LQ7,LQSV,LQAN,LQDW,LQUP         +SQ
      COMMON /QUEST/ IRET(100)

      PARAMETER (NHTOZQ = 7)

      DIMENSION    VECT(*)


  900 CONTINUE

      IF   (MODE)            98, 11, 41
   11 LQWORK = LQWORG
      IGARB  = 0


   12 IF (LQSTA.GE.LQEND)                                  GO TO 93
      LQMTB = LQWORK + 3
      LMO   = LQWORK - 3

      NDEADW = 0
      NBKLIV = 0
      LAST   = 7
      LAT    = LQSTA
      LNX    = LQSTA


   21 LBK = LNX
      CALL QBLOWX (LBK)
      IF (IQFOUL.NE.0)                                     GO TO 91
      LNX = IQLS + IQND + 1
      IF (JBIT(LQ(IQLS),IQDROP).NE.0)                      GO TO 25


      IF (LAST.EQ.0)                                       GO TO 22
      LAST = 0
      LMO  = LMO + 3
      LQ(LMO) = LBK

   22 NBKLIV = NBKLIV + 1
      LQ(LAT-1) = LBK
      LQ(LAT-2) = LNX
      LQ(LAT-3) = IQNL + 2
      LAT = LAT - 3
      IF (LAT.LT.LMO)                                      GO TO 37
      IF (LNX.LT.LQEND)                                    GO TO 21
                                                           GO TO 31


   25 IF (LAST.NE.0)                                       GO TO 26
      LAST = 7
      LQ(LMO+1) = LBK

   26 NDEADW = NDEADW + (LNX-LBK)
      IF (LNX.LT.LQEND)                                    GO TO 21


   31 IF (NBKLIV.EQ.0)                                     GO TO 93

      IF (LAST.EQ.0)  LQ(LMO+1)=LQEND
      LQMTE = LMO - 3

      LQ(LAT-1) = LQ(LAT+1) - 7
      LQTB = LAT - 1

      LQTA  = LMO + 3
      IF (LQTA+3.GE.LQTB)                                  GO TO 37

      IRET(1) = 0
      IRET(2) = (LQEND-LQSTA) - NDEADW + NHTOZQ*NBKLIV
      RETURN


   37 IF (IGARB.NE.0)                                      GO TO 92
      IGARB = 7
      LSTASV = LQSTA
      CALL MQGARB
      IF (LQSTA.EQ.LSTASV)                                 GO TO 92
                                                           GO TO 12


   41 LAT1 = LQTB
      LAT2 = LQSTA - 1
      NWAT = LQSTA - LAT1
      CALL UCOPIV (LQ(LAT1),VECT,NWAT)
      CALL UCOPY  (VECT,LQ(LAT1),NWAT)


      LQTB = LQTA + 3
      LTB  = LQTB
      LAT  = LAT1
      NREL = LNSTA - LQ(LAT1) - 1


   51 LBK = LQ(LAT)
      LNX = LQ(LAT+1)

      LQ(LTB)   = LBK
      LQ(LTB+1) = LNX
      LQ(LTB+2) = NREL

      LTB = LTB + 3
      LAT = LAT + 3
      NREL = NREL + NHTOZQ - (LQ(LAT)-LNX)
      IF (LAT.LT.LAT2)                                     GO TO 51

      LQ(LTB) = LQEND
      LQTE = LTB - 3

      LQ(LQTB-1) = 7
      LQ(LQTB-2) = LQSTA

      CALL MQRELC


      NW = LQTE+4 - LAT1
      IF (NW.GT.0)  CALL UCOPY (VECT,LQ(LAT1),NW)

      LTARG = LOCF(VECT(1)) - (LOCF(Q(1)) - 1)
      NREL  = LTARG - LQ(LAT1)

      LAT = LAT1


   61 LBK = LQ(LAT)
      LNX = LQ(LAT+1)
      NW  = LQ(LAT+2)
      LST = LBK + NW
      CALL UCOPY (LQ(LBK),LQ(LBK+NREL),NW)
   64 NREL = NREL + NHTOZQ

      NDEADW = LQ(LAT+3) - LNX
      IF (NDEADW.NE.0)                                     GO TO 66


      NW = (LNX-LST) + LQ(LAT+5)
      CALL UCOPY (LQ(LST),LQ(LST+NREL),NW)

      LAT = LAT + 3
      LBK = LQ(LAT)
      LNX = LQ(LAT+1)
      LST = LQ(LAT+2) + LBK
                                                           GO TO 64


   66 NW = LNX - LST
      CALL UCOPY (LQ(LST),LQ(LST+NREL),NW)

      NREL = NREL - NDEADW
      LAT  = LAT + 3
      IF (NDEADW.GE.0)                                     GO TO 61

      IRET(1) = 0
      IRET(2) = LQMAIN
      IRET(3) = IQ(LQF)
                                                           GO TO 98


   91 IRET(1) = 1
                                                           GO TO 98

   92 IRET(2) = 2
                                                           GO TO 98

   93 IRET(2) = 3

   98 LQSTA  = LQEND
      LQMAIN = 0
  999 RETURN
      END
