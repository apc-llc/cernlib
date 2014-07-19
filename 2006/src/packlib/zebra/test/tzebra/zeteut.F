*
* $Id: zeteut.F,v 1.2 1996/04/18 16:14:46 mclareni Exp $
*
* $Log: zeteut.F,v $
* Revision 1.2  1996/04/18 16:14:46  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE ZETEUT

#include "zebra/zbcd.inc"
#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzct.inc"
#include "test_include/cqc.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    End CDE                             --------------


C     IQCOPT(2)= 7
C     NQDEVZ = 7

      CALL CQHEAD (3,'UTY ', 'TEST UTILITIES.','-','-')

      CALL MZSTOR (IXSTOR,'//', '.', FENCE
     +,            LQ,LA1,LX1,LQ(NQLIM2),IQ(19997))

      CALL CQFIXU (1,17000)
      CALL MZWORK (IXSTOR,M,DLAST,0)
      CALL VZERO  (LQUSER,150)

C     CALL MZLINK (IXSTOR,'/LAREX/', LAREX, LAREX(12),LAREX)
C     CALL MZLINK (IXSTOR,'/LAREY/', LAREY, LAREY,    LAREY(12))
C     CALL MZLINT (IXSTOR,'/LAREZ/',IFLREZ, LAREZ(7), LAREZ(12))

C-                 Lift structure

      CALL CQHEAD (1,'-', 'ORIGINAL STRUCTURE.', '-','-')
      CALL MZSDIV (0,-1)
      CALL CQLODS (0,0)

      CALL ZSHUNT (IXSTOR,LB1,LA6,0,1)
      CALL ZSHUNT (IXSTOR,LC1,LB4,0,1)

      DO 39  JBK=1,14
      J2 = (JBK-1)/2
      J4 = (JBK+1)/4
      L  = LV(JBK)

      IQ(L+11) = J4
      IQ(L+12) = J2 - 3
      IQ(L+13) = JBK- 16
      IQ(L+14) = JBK

      IQ(L+15) =  1 - J4
      IQ(L+16) =  2 - J2
      IQ(L+17) = 16 - JBK
      IQ(L+18) = -JBK

      DO 36  JF=1,10
      I = IQ(L+JF+10)
      Q(L+JF+20) = I

   34 IF (I.GT.0)                  GO TO 35
      I = I + 20
      GO TO 34

   35 IF (I.GE.24)   I=MOD(I,20) + 1
      CALL UBUNCH (IQLETT(I),IQ(L+JF+30),4)
   36 CONTINUE
   39 CONTINUE

      CALL CQDLIN (LA1,11,10,1)
      CALL CQDLIN (LA1,31,10,2)

C----              Test ZSORTI

      CALL CQHEAD (2,'-', 'TEST ZSORTI.', '-','-')

C--                Positive key only

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORTI (IXSTOR,LQ(LB4),14)
      CALL CQDLIN (LQ(LB4),14,1,1)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORTI (IXSTOR,LQ(LB4),12)
      CALL CQDLIN (LQ(LB4),12,1,1)

C--                Negative key only

      CALL ZSORTI (IXSTOR,LQ(LB4),16)
      CALL CQDLIN (LQ(LB4),16,1,1)

      CALL ZSORTI (IXSTOR,LQ(LA6),17)
      CALL CQDLIN (LQ(LA6),17,1,1)

C--                Mixed keys

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORTI (IXSTOR,LQMAIN, 12)
      CALL CQDLIN (LQMAIN, 12,1,1)
      CALL ZSORTI (IXSTOR,LQMAIN, 14)

C----              Test ZSORT

      CALL CQHEAD (2,'-', 'TEST ZSORT .', '-','-')

C--                Positive key only

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORT  (IXSTOR,LQ(LB4),24)
      CALL CQDLIN (LQ(LB4),24,1,0)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORT  (IXSTOR,LQ(LB4),22)
      CALL CQDLIN (LQ(LB4),22,1,0)

C--                Negative key only

      CALL ZSORT  (IXSTOR,LQ(LB4),26)
      CALL CQDLIN (LQ(LB4),26,1,0)

      CALL ZSORT  (IXSTOR,LQ(LA6),27)
      CALL CQDLIN (LQ(LA6),27,1,0)

C--                Mixed keys

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORT  (IXSTOR,LQMAIN, 22)
      CALL CQDLIN (LQMAIN ,22,1,0)
      CALL ZSORTI (IXSTOR,LQMAIN, 14)

C----              Test ZSORTH

      CALL CQHEAD (2,'-', 'TEST ZSORTH.', '-','-')

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORTH (IXSTOR,LQ(LB4),32)
      CALL CQDLIN (LQ(LB4),32,1,2)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORTH (IXSTOR,LQ(LB4),33)
      CALL CQDLIN (LQ(LB4),33,1,2)

      CALL ZSORTH (IXSTOR,LQ(LB4),36)
      CALL CQDLIN (LQ(LB4),36,1,2)

      CALL ZSORTH (IXSTOR,LQ(LB4),37)
      CALL CQDLIN (LQ(LB4),37,1,2)

      CALL ZSORTH (IXSTOR,LQ(LA6),38)
      CALL CQDLIN (LQ(LA6),38,1,2)

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORTH (IXSTOR,LQMAIN, 34)
      CALL CQDLIN (LQMAIN, 34,1,2)

C----              Test ZSORVI

      CALL CQHEAD (2,'-', 'TEST ZSORVI.', '-','-')

C--                Positive first key only

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORVI (IXSTOR,LQ(LB4),14,2)
      CALL CQDLIN (LQ(LB4),14,2,1)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORVI (IXSTOR,LQ(LB4),11,2)
      CALL CQDLIN (LQ(LB4),11,2,1)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORVI (IXSTOR,LQ(LB4),11,3)
      CALL CQDLIN (LQ(LB4),11,3,1)

C--                Negative first key only

      CALL ZSORVI (IXSTOR,LQ(LB4),15,2)
      CALL CQDLIN (LQ(LB4),15,2,1)

      CALL ZSORVI (IXSTOR,LQ(LB4),15,3)
      CALL CQDLIN (LQ(LB4),15,3,1)

      CALL ZSORVI (IXSTOR,LQ(LA6),16,2)
      CALL CQDLIN (LQ(LA6),16,2,1)

C--                Mixed keys

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORVI (IXSTOR,LQMAIN, 12,2)
      CALL CQDLIN (LQMAIN,12,2,1)

C----              Test ZSORV

      CALL CQHEAD (2,'-', 'TEST ZSORV .', '-','-')

C--                Positive first key only

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORV  (IXSTOR,LQ(LB4),24,2)
      CALL CQDLIN (LQ(LB4),24,2,0)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORV  (IXSTOR,LQ(LB4),21,2)
      CALL CQDLIN (LQ(LB4),21,2,0)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORV  (IXSTOR,LQ(LB4),21,3)
      CALL CQDLIN (LQ(LB4),21,3,0)

C--                Negative first key only

      CALL ZSORV  (IXSTOR,LQ(LB4),25,2)
      CALL CQDLIN (LQ(LB4),25,2,0)

      CALL ZSORV  (IXSTOR,LQ(LB4),25,3)
      CALL CQDLIN (LQ(LB4),25,3,0)

      CALL ZSORV  (IXSTOR,LQ(LA6),26,2)
      CALL CQDLIN (LQ(LA6),26,2,0)

C--                Mixed keys

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORV  (IXSTOR,LQMAIN, 22,2)
      CALL CQDLIN (LQMAIN,22,2,0)

C----              Test ZSORVH

      CALL CQHEAD (2,'-', 'TEST ZSORVH.', '-','-')

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORVH (IXSTOR,LQ(LB4),31,2)
      CALL CQDLIN (LQ(LB4),31,2,2)

      CALL ZSHUNT (IXSTOR,LC2,LC4,0,0)
      CALL ZSORVH (IXSTOR,LQ(LB4),31,3)
      CALL CQDLIN (LQ(LB4),31,3,2)

      CALL ZSORVH (IXSTOR,LQ(LB4),35,2)
      CALL CQDLIN (LQ(LB4),35,2,2)

      CALL ZSORVH (IXSTOR,LQ(LB4),35,3)
      CALL CQDLIN (LQ(LB4),35,3,2)

      CALL ZSORVH (IXSTOR,LQ(LA6),36,2)
      CALL CQDLIN (LQ(LA6),36,2,2)

      CALL ZSHUNT (IXSTOR,LA2,LA3,0,0)
      CALL ZSORVH (IXSTOR,LQMAIN, 33,2)
      CALL CQDLIN (LQMAIN,33,2,2)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
