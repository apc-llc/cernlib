*
* $Id: cqstom.F,v 1.2 1996/04/18 16:14:26 mclareni Exp $
*
* $Log: cqstom.F,v $
* Revision 1.2  1996/04/18 16:14:26  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQSTOM (NOCHK)

C-    Map the current store

#include "test_include/cqc.inc"
#include "zebra/zbcd.inc"
#include "zebra/mzcn.inc"
#include "zebra/mqsys.inc"
C--------------    End CDE                             --------------
      DIMENSION    IV(16)
      EQUIVALENCE (LS,IQLS), (LNX,IQNX)

#include "zebra/q_jbyt.inc"


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      CALL UCOPY (NQPNAM(KQT+1),NQSNAM,5)

      CALL CQHEAD (1,  '.', 'MAP CURRENT STORE  -----.', 'HJIIIII'
     +,            'NAME,JQSTOR,/JQDIVI,/KQT,/KQS,/LQSTOR.')

      IV(1) = NQSNAM(1)
      IV(2) = NQSNAM(2)
      IV(3) = JQSTOR
      IV(4) = JQDIVI
      IV(5) = KQT
      IV(6) = KQS
      IV(7) = LQSTOR
      CALL CQDATA (IV(1),IV(7))

      CALL CQHEAD (0,  '.', '.', 'JJJJJJJ'
     +, 'NQFEND,NQSTRU,NQREF,NQLINK,*NQMINR,*LQ2END,JQDVLL,JQDVSY.')

      CALL CQDATA (NQFEND,JQDVSY)

      JDV = 0
   21 JDV = JDV + 1

      CALL CQHEAD (1,  '.', 'NEXT DIVISION.', 'JJOOOOH'
     +, 'JDIV,LQSTA,LQEND,NQDMAX,IQMODE,IQKIND,IQRTO,IQRNO,NAME.')

      IV(1) = JDV
      IV(2) = LQSTA(KQT+JDV)
      IV(3) = LQEND(KQT+JDV)
      IV(4) = NQDMAX(KQT+JDV)
      IV(5) = IQMODE(KQT+JDV)
      IV(6) = IQKIND(KQT+JDV)
      IV(7) = IQRTO(KQT+JDV)
      IV(8) = IQRNO(KQT+JDV)
      IV(9) = IQDN1(KQT+JDV)
      IV(10)= IQDN2(KQT+JDV)
      CALL CQDATA (IV(1),IV(10))

      LSTA = IV(2)
      LEND = IV(3)
      IF (LSTA.GE.LEND)            GO TO 69

      CALL CQHEAD (0,  '.', '.', 'PJJBJJJ'
     +, 'STATS,STATU,KADR,LUP,LS,IDN,IDH,NEXT,NL,NS,ND,LINK0,LINK1,
     +LINK2.')

      LNX = LSTA
   31 LN  = LNX
      CALL MZCHLN (-7,LN)
      IF (IQFOUL.NE.0)             GO TO 91
      IF (IQND.LT.0)               GO TO 39

      IV(1)  = JBYT (IQ(KQS+LS),IQDROP,8)
      IV(2)  = JBYT (IQ(KQS+LS),1,15)
      IV(3)  = LQ(KQS+LS+2)
      IV(4)  = LQ(KQS+LS+1)
      IV(5)  = LS
      IV(6)  = IQ(KQS+LS-5)
      IV(7)  = IQ(KQS+LS-4)

      IV(8)  = IQBLAN
      L  = LQ(KQS+LS)
      IF (L.EQ.0)                  GO TO 33
      IV(8)  = IQ(KQS+L-4)

   33 IV(9)  = IQNL
      IV(10) = IQNS
      IV(11) = IQND
      IV(12) = LQ(KQS+LS)
      IV(13) = LQ(KQS+LS-1)
      IV(14) = LQ(KQS+LS-2)
      N      = 12 + MIN(IQNL,2)
      CALL CQDATA (IV(1),IV(N))
   39 IF (LNX.LT.LEND)             GO TO 31
   69 IF (JDV.EQ.JQDVLL)  JDV=JQDVSY-1
      IF (JDV.LT.20)               GO TO 21
      IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN

C----              Bank chaining clobbered

   91 CONTINUE
      CALL ZFATAM ('CQSTOM, BANK CHAINING CLOBBERED.')
      END
*      ==================================================
#include "zebra/qcardl.inc"
