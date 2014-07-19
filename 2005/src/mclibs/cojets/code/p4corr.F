*
* $Id: p4corr.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: p4corr.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE P4CORR(W,PMB,NP,IFLAG)
C     *********************************
C-- CORRECT TO HAVE ENERGY-MOMENTUM CONSERVATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
      DIMENSION TM2V(MAXJTP)
      DIMENSION SUM(2),PMB(4)
C
      IFLAG=0
      WSQ=W**2
C-- TRANSVERSE MOMENTUM, FIRST
      DO 81 J=1,2
      SUM(J)=-PMB(J)
      DO 82 I=1,NP
82    SUM(J)=SUM(J)+P(I,J)
81    SUM(J)=SUM(J)/NP
      WMIN=0.
      DO 83 I=1,NP
      DO 84 J=1,2
84    P(I,J)=P(I,J)-SUM(J)
      TM2V(I)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      WMIN=WMIN+SQRT(TM2V(I))
83    CONTINUE
      IF(W.LT.WMIN) GO TO 90
C-- LONGITUDINAL MOMENTUM AND ENERGY
      SUM3=0.
      SUM4=0.
      DO 85 I=1,NP
      SUM3=SUM3+P(I,3)
      P(I,4)=SQRT(P(I,3)**2+TM2V(I))
85    SUM4=SUM4+P(I,4)
      F3=SUM3/SUM4
      SUM3=0.
      DO 86 I=1,NP
      P(I,3)=P(I,3)-F3*P(I,4)
86    SUM3=SUM3+ABS(P(I,3))
      WMINSQ=WMIN**2
      FF=WSQ-WMINSQ
      F2=SQRT(FF)/SUM3
      DO 87 IT=1,20
      TEST=0.
      SUM3=0.
      DO 88 I=1,NP
      P(I,3)=F2*P(I,3)
      SUM3=SUM3+ABS(P(I,3))
      P(I,4)=SQRT(P(I,3)**2+TM2V(I))
      TEST=TEST+P(I,4)
88    CONTINUE
      IF(ABS(TEST-W).LT.0.01) GO TO 89
      F2=W/TEST
      GG=TEST**2-WMINSQ
      IF(GG.GT.0.) F2=SQRT(FF/GG)
87    CONTINUE
90    CONTINUE
      IFLAG=1
89    CONTINUE
      RETURN
      END
