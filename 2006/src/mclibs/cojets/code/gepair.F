*
* $Id: gepair.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: gepair.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GEPAIR
C     *****************
C-- GENERATES PAIR OF INITIAL PARTONS - CALLED BY GEVENT
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
#include "cojets/tabqrk.inc"
#include "cojets/tweigh.inc"
      DIMENSION WQQ(6),WQAQ(3)
      INTEGER INV(2)
      DATA ICALL/0/ ,WQAQ/3*0./ ,INV/2,1/
C-- INITIALIZATION
      IF(ICALL.GT.0) GO TO 9
      ICALL=1
      DO 8 J=1,7
      DO 7 IX=2,NBIN
7     TABQRK(IX,J)=TABQRK(IX-1,J)+TABQRK(IX,J)
      WGQV(J)=TABQRK(NBIN,J)
8     CONTINUE
      WGQV(5)=WGQV(5)*.5
      WGQV(6)=WGQV(5)
      DO 1 J=1,6
1     WQQ(J)=WGQV(J)**2
      DO 2 J=2,6
2     WQQ(J)=WQQ(J)+WQQ(J-1)
      DO 3 J=1,6
3     WQQ(J)=WQQ(J)/WQQ(6)
      DO 6 J=1,6,2
      JN=(J+1)/2
6     WQAQ(JN)=WGQV(J)*WGQV(J+1)
      WQAQ(2)=WQAQ(1)+WQAQ(2)
      WQAQ(3)=WQAQ(2)+WQAQ(3)
      WQAQ(1)=WQAQ(1)/WQAQ(3)
      WQAQ(2)=WQAQ(2)/WQAQ(3)
      WQAQ(3)=1.
      DO 4 J=2,6
4     WGQV(J)=WGQV(J)+WGQV(J-1)
      DO 5 J=1,6
5     WGQV(J)=WGQV(J)/WGQV(6)
      XST=XMIN
      ALST=LOG(XST)
      IXST=INT(XST/DBIN)+1
      ALST1=LOG(FLOAT(IXST)*DBIN)
9     CONTINUE
C-- CHANNEL BRANCHING - TRANSLATE INTO PARTON FLAVORS
C-- IPACH = 1 Q-Q OR Q-QB DIFFERENT FLAVORS, 2 Q-Q SAME FLAVORS,
C--         3 Q-QB SAME FLAVORS, 4 Q-G, 5 G-G
      IF(IPACH.EQ.1) GO TO 10
      IF(IPACH.EQ.2) GO TO 20
      IF(IPACH.EQ.3) GO TO 30
      IF(IPACH.EQ.4) GO TO 40
      IF(IPACH.EQ.5) GO TO 50
10    CONTINUE
      DO 11 L=1,2
14    RR=CJRN(L)
      DO 12 J=1,6
      JOUT=J
      IF(RR.LT.WGQV(J)) GO TO 13
12    CONTINUE
13    ICHING(L)=JOUT
      IF(L.EQ.1) GO TO 11
      CALL GECODE(ICHING,IFLING)
11    CONTINUE
      IF(ABS(IFLING(1)).EQ.ABS(IFLING(2))) GO TO 10
      GO TO 60
20    IF(IPBAR.EQ.1) GO TO 35
25    CONTINUE
      RR=CJRN(0)
      DO 21 J=1,6
      JOUT=J
      IF(RR.LT.WQQ(J)) GO TO 22
21    CONTINUE
22    ICHING(1)=JOUT
      ICHING(2)=JOUT
      CALL GECODE(ICHING,IFLING)
      GO TO 60
30    IF(IPBAR.EQ.1) GO TO 25
35    CONTINUE
      L1=1.5+CJRN(0)
      L2=INV(L1)
      RR=CJRN(0)
      DO 31 J=1,3
      JOUT=J
      IF(RR.LT.WQAQ(J)) GO TO 32
31    CONTINUE
32    ICHING(L1)=2*JOUT-1
      ICHING(L2)=ICHING(L1)+1
      CALL GECODE(ICHING,IFLING)
      GO TO 60
40    L1=1.5+CJRN(0)
      L2=INV(L1)
      ICHING(L1)=7
      RR=CJRN(0)
      DO 41 J=1,6
      JOUT=J
      IF(RR.LT.WGQV(J)) GO TO 42
41    CONTINUE
42    ICHING(L2)=JOUT
      CALL GECODE(ICHING,IFLING)
      GO TO 60
50    ICHING(1)=7
      ICHING(2)=7
      IFLING(1)=LGLUS
      IFLING(2)=LGLUS
C-- GENERATE X'S AND PT'S FOR INITIAL PARTON PAIR
60    CONTINUE
      DO 71 L=1,2
      CALL GEXIN(ICHING(L),XST,IXST,ALST,ALST1,XING(L),XMING(L)
     1 ,PXING(L),PYING(L))
71    CONTINUE
C-- CHECK WHETHER SHAT IS ENOUGH FOR QSQ
      SHAT=(XING(1)*XING(2)+XMING(1)*XMING(2))*S
     1 -2.*(PXING(1)*PXING(2)+PYING(1)*PYING(2))
      IF(SHAT.LT.3.*QSQ) GO TO 9
      RETURN
      END
