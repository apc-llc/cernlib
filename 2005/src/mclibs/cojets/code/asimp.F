*
* $Id: asimp.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: asimp.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ASIMP(A1,B1,EP,M,N,FUN)
C     ********************************
C-- ADAPTIVE SIMPSON INTEGRATION ROUTINE
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION F2(30),FBP(30),EST2(30)
      INTEGER NRTR(30)
      DIMENSION AEST2(30),FTST(3),XB(30)
      IF(N.LE.0) GO TO 210
      IF(N.GT.3) GO TO 211
      A=A1
      B=B1
      EPS=EP*15.
      ESUM=0.
      TSUM=0.
      LVL=1
      DA=B-A
      FA=FUN(A)
      FM=FUN((A+B)*0.5)
      FB=FUN(B)
      M=3
      FMAX=ABS(FA)
      FTST(1)=FMAX
      FTST(2)=ABS(FM)
      FTST(3)=ABS(FB)
      DO 100 I=2,3
      IF(FMAX.GE.FTST(I)) GO TO 100
      FMAX=FTST(I)
100   CONTINUE
      EST=(FA+4.*FM+FB)*DA/6.
      ABSAR=(FTST(1)+4*FTST(2)+FTST(3))*DA/6.
      AEST=ABSAR
C     1=RECUR
1     SX=(DA/(2.**LVL))/6.
      F1=FUN((3.*A+B)/4.)
      F2(LVL)=FUN((A+3.*B)/4.)
      EST1=SX*(FA+4.*F1+FM)
      FBP(LVL)=FB
      XB(LVL)=B
      EST2(LVL)=SX*(FM+4.*F2(LVL)+FB)
      SUM=EST1+EST2(LVL)
      FTST(1)=ABS(F1)
      FTST(2)=ABS(F2(LVL))
      FTST(3)=ABS(FM)
      AEST1=SX*(ABS(FA)+4.*FTST(1)+FTST(3))
      AEST2(LVL)=SX*(FTST(3)+4.*FTST(2)+ABS(FB))
      ABSAR=ABSAR-AEST+AEST1+AEST2(LVL)
      M=M+2
      IF(N.EQ.1) GO TO 201
      IF(N.EQ.2) GO TO 200
      IF(N.EQ.3) GO TO 202
200   DELTA=ABSAR
      GO TO 205
210   WRITE(ITLIS,39)
39    FORMAT(20H ERROR RETURN-N.LT.0     )
      RETURN
211   WRITE(ITLIS,40)
40    FORMAT(20H ERROR RETURN-N.GT.3     )
      RETURN
201   DELTA=1.
      GO TO 205
202   DO 203 I=1,2
      IF(FMAX.GE.FTST(I)) GO TO 203
      FMAX=FTST(I)
203   CONTINUE
      DELTA=FMAX
205   DAFT=EST-SUM
      DIFF=ABS(DAFT)
      DAFT=DAFT/15.
      IF(DIFF-EPS*DELTA)6,6,3
3     IF(LVL-30)4,2,2
6     IF(LVL-1)2,4,2
C     2=UP
2     A=B
      ESUM=ESUM+DAFT
      TSUM=TSUM+SUM
9     LVL=LVL-1
      L=NRTR(LVL)
      IF(L.EQ.1) GO TO 11
      IF(L.EQ.2) GO TO 12
C     11=R1,12=R2
4     NRTR(LVL)=1
      EST=EST1
      AEST=AEST1
      FB=FM
      FM=F1
      B=(A+B)/2.
      EPS=EPS/2.
7     LVL=LVL+1
      GO TO 1
11    NRTR(LVL)=2
      FA=FB
      FM=F2(LVL)
      FB=FBP(LVL)
      B=XB(LVL)
      EST=EST2(LVL)
      AEST=AEST2(LVL)
      GO TO 7
12    EPS=2.*EPS
      IF(LVL-1)5,5,9
5     ASIMP=TSUM-ESUM
      RETURN
      END
