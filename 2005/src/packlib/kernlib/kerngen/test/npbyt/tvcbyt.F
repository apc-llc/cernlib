*
* $Id: tvcbyt.F,v 1.1.1.1 1996/02/15 17:54:59 mclareni Exp $
*
* $Log: tvcbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:59  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TVCBYT

#include "kerngent/mkcde.inc"

      INTEGER XA
      DIMENSION MPACK1(3),MPACK2(3)

      CALL NEWGUY ('JBYTPK - SBYTPK - PKBYT - UPKBYT - INCBYT.'
     +,            'TVCBYT  ')

C--                PREPARE USEFUL CONSTANTS
      NBITCH=IQBITW/IQCHAW
      KMAX=2**NBITCH-1
      IBIT2=IQBITW+IQBITW

      MPACK1(1)=NBITCH
      MPACK1(2)=IQCHAW
      MPACK1(3)=KMAX

      NBIT2=NBITCH+1
      MPACK2(1)=NBIT2
      MPACK2(2)=IQBITW/NBIT2
      MPACK2(3)=KMAX+KMAX+1
      KMAX1=KMAX/2

C--                PREPARE THE TEST VECTOR  (IB)
      XA=0
         DO 5 I=1,50
      IB(I)=XA
      IB(I+150)=0
      IB(I+100)=XA+XA
      IF (IB(I+100).LE.KMAX1) GO TO 4
      IB(I+150)=IB(I+100)-KMAX1
      IB(I+100)=KMAX1
    4 XA=XA+I
      IF (XA.GT.KMAX) XA=0
    5 CONTINUE

      XA=0
         DO 8 I=1,IBIT2
      IB(I+202)=XA
      XA=XA+1
      IF (XA.EQ.2) XA=0
    8 CONTINUE

C--                FIRST CHECKS OF ROUTINES
      CALL UZERO (IA,1,200)
      XA=0
         DO 10 I=1,50
      IF (XA.GT.KMAX) XA=0
      CALL SBYTPK (XA,IA,I,MPACK1)
      CALL SBYTPK (XA,IA(51),I,MPACK2)
   10 XA=XA+I

      XA=0
         DO 12 I=1,IBIT2
      CALL SBYTPK (XA,IA(201),I,0)
      XA=XA+1
      IF (XA.EQ.2) XA=0
   12 CONTINUE

         DO 15 I=1,50
      IA(I+100)=JBYTPK (IA,I,MPACK1)
      IA(I+150)=JBYTPK (IA(51),I,MPACK2)
   15 CONTINUE

         DO 18 I=1,IBIT2
      IA(I+202)=JBYTPK (IA(201),I,0)
   18 CONTINUE
      CALL MVERII (1,IA(101),IB,50)
      CALL MVERII (2,IA(151),IB,50)
      CALL MVERII (3,IA(203),IB(203),IBIT2)

C--                SECOND CHECKS OF ROUTINES
      NW1=49/IQCHAW+1
      NW2=49/MPACK2(2) +1
      CALL UCOPY (IA,IB(51),NW1)
      CALL UCOPY (IA(51),IB(NW1+51),NW2)
      CALL UZERO (IA,1,100)

      CALL UPKBYT (IB(51),1,IA,25,MPACK1)
      CALL UPKBYT (IB(51),26,IA(26),25,MPACK1)
      CALL PKBYT (IB,IA(51),1,25,MPACK1)
      CALL PKBYT (IB(26),IA(51),26,25,MPACK1)
      CALL MVERII (4,IA,IB,50+NW1)

      CALL UCOPY (IB(NW1+51),IB(51),NW2)
      CALL UZERO (IA,1,100)
      CALL UPKBYT (IB(51),1,IA,50,MPACK2)
      CALL PKBYT (IB,IA(51),1,50,MPACK2)
      CALL MVERII (5,IA,IB,50+NW2)

      IB(201)=IA(201)
      IB(202)=IA(202)
      CALL UZERO (IA,1,300)
      CALL UPKBYT (IB(201),1,IA(203),IBIT2,0)
      CALL PKBYT (IB(203),IA(201),1,IBIT2,0)
      CALL MVERII (6,IA(201),IB(201),IBIT2+2)

C--                THIRD CHECK OF ROUTINE
      MPACK1(3)=KMAX1
      CALL PKBYT (IB,IA(101),1,50,MPACK1)
      XA=0
         DO 30 I=1,50
      IA(I+50)=INCBYT (XA,IA(101),I,MPACK1)
      XA=XA+I
      IF (XA.GT.KMAX) XA=0
   30 CONTINUE
      CALL UPKBYT (IA(101),1,IA,50,MPACK1)
      CALL MVERII (7,IA,IB(101),100)

C--                CHECK PKBYT NON-DESTRUCTIVE

      MPACK1(1) = 4                                                      HEX
C     MPACK1(1) = 3                                                     -HEX
      MPACK1(2) = 7
      CALL VFILL (IA,30,-7)

      JTH  = 0
      JVAL = 0

      DO  43  JW=1,6
      JPOS = 1

      DO  42  JB=1,7
      CALL SBYT (JVAL,IA(JW+10),JPOS,MPACK1(1))
      JTH  = JTH  + 1
      IA(JTH+100) = JVAL
      JPOS = JPOS + MPACK1(1)
   42 JVAL = JVAL + 1
      CALL SBIT0 (IA(JW),30)
      CALL SBIT0 (IA(JW+10),30)
   43 CONTINUE

      CALL PKBYT (IA(101),IA,1,42,MPACK1)
      CALL MVERII (11,IA,IA(11),6)

      CALL PKBYT (IA(103),IA,3, 2,MPACK1)
      CALL MVERII (12,IA,IA(11),6)

      CALL PKBYT (IA(103),IA,3, 5,MPACK1)
      CALL MVERII (13,IA,IA(11),6)

      CALL PKBYT (IA(103),IA,3, 6,MPACK1)
      CALL MVERII (14,IA,IA(11),6)

      CALL PKBYT (IA(109),IA,9, 2,MPACK1)
      CALL MVERII (15,IA,IA(11),6)

      CALL PKBYT (IA(109),IA,9,12,MPACK1)
      CALL MVERII (16,IA,IA(11),6)
      RETURN

      END
