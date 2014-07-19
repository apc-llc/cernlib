*
* $Id: intid.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: intid.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION INTID(ID)
C     ******************
C-- GIVES INTERNAL CODE OF PARTICLE WITH ISAJET CODE ID
C-- CREATED: 88/04/27
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION LEPT(12),MES0(36),MES1(36)
      DATA LEPT/ 9,10, 7, 8,13,14,11,12,17,18,15,16/
      DATA MES0/51,21,22,52,23,24,25,26,53,28,27,30
     1         ,29,32,31,54,34,33,36,35,38,37,40,39
     2         ,55,42,41,44,43,46,45,48,47,50,49,56/
      DATA MES1/91,61,62,92,63,64,65,66,93,68,67,70
     1         ,69,72,71,94,74,73,76,75,78,77,80,79
     2         ,95,82,81,84,83,86,85,88,87,90,89,96/
C
      INTID=0
      IDA=ABS(ID)
      IF(MOD(IDA,100).EQ.0) RETURN
      IF(IDA.GT.100) GO TO 10
      IF(IDA.GE.11.AND.IDA.LE.16) GO TO 10
      IF(MOD(IDA,10).NE.0) RETURN
      IF(IDA.GT.20.AND.IDA.LT.80) RETURN
      IF(ID.EQ. 10) INTID= 1
      IF(ID.EQ. 20) INTID=57
      IF(ID.EQ.-20) INTID=58
      IF(ID.EQ. 80) INTID= 3
      IF(ID.EQ.-80) INTID= 4
      IF(ID.EQ. 90) INTID= 2
      RETURN
C-- BREAK DOWN OF ID
10    CONTINUE
      ISGN=1
      IF(ID.LT.0) ISGN=2
      I=IDA/1000
      J=MOD(IDA/100,10)
      K=MOD(IDA/10,10)
      JSPIN=MOD(IDA,10)
      IF(IDA.GT.100) GO TO 50
C-- LEPTONS
      INTID=LEPT(2*(JSPIN-1)+ISGN)
      RETURN
50    IF(IDA.GT.1000) GO TO 100
C-- MESONS
      IF(J.GT.K) RETURN
      IF(J.EQ.K.AND.ISGN.EQ.2) RETURN
      L=(K-1)**2+2*(J-1)+ISGN
      IF(JSPIN.EQ.0) INTID=MES0(L)
      IF(JSPIN.EQ.1) INTID=MES1(L)
      RETURN
100   IF(JSPIN.EQ.1) GO TO 150
C-- J=1/2 BARYONS
      IF(MAX(I,J).GT.K) RETURN
      IF(I.EQ.J.AND.J.EQ.K) RETURN
      INTID=100+(2*(K-2)*(K-1)*K)/3
      IF(J.NE.K) THEN
        IJX=MAX(I,J)
        IJM=MIN(I,J)
        INV=0
        IF(I.GT.J) INV=1
        INTID=INTID+2*(IJX-1)**2+4*(IJM-1)+2*INV+ISGN
      ELSE
        INTID=INTID+2*(K-1)**2+2*(I-1)+ISGN
      ENDIF
      RETURN
150   CONTINUE
C-- J=3/2 BARYONS
      IF(I.GT.J.OR.J.GT.K) RETURN
      INTID=240+((K-1)*K*(K+1))/3+(J-1)*J+2*(I-1)+ISGN
      RETURN
      END
