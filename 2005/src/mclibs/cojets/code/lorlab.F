*
* $Id: lorlab.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: lorlab.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE LORLAB(PIN,POUT,NEW)
C     *******************************
C-- TRANSORMS FROM COLLIDING PARTON C.M. SYSTEM TO LABORATORY
C-- (COLLIDING HADRON C.M.) SYSTEM
C-- LORENTZ TRANSFORMATION DEFINED BY LEAVING COMPONENTS ORTHOGONAL
C-- TO INITIAL PARTON COLLISION PLANE UNALTERED
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/entrev.inc"
#include "cojets/itapes.inc"
#include "cojets/parq.inc"
      DIMENSION PIN(4),POUT(4)
      DIMENSION A(4),B(4),AM(4),BCM(4),PM(4)
C
      IF(NEW.EQ.0) GO TO 100
      NEW=0
      ECM=SQRT(S)
      A(1)=PARACT(1,6,1)+PARACT(1,6,2)
      A(2)=PARACT(1,7,1)+PARACT(1,7,2)
      A(3)=(PARACT(1,5,1)-PARACT(1,3,1)
     *     -PARACT(1,5,2)+PARACT(1,3,2))*ECM*.5
      A(4)=(PARACT(1,5,1)+PARACT(1,3,1)
     *     +PARACT(1,5,2)+PARACT(1,3,2))*ECM*.5
      B(1)=PARACT(1,6,1)-PARACT(1,6,2)
      B(2)=PARACT(1,7,1)-PARACT(1,7,2)
      B(3)=(PARACT(1,5,1)-PARACT(1,3,1)
     *     +PARACT(1,5,2)-PARACT(1,3,2))*ECM*.5
      B(4)=(PARACT(1,5,1)+PARACT(1,3,1)
     *     -PARACT(1,5,2)-PARACT(1,3,2))*ECM*.5
      DO 1 J=1,3
1     AM(J)=-A(J)
      AM(4)=A(4)
      CALL CJLORN(AM,B,BCM,1)
      BCMT2=BCM(1)**2+BCM(2)**2
      IF(BCMT2.LT.1.E-30) GO TO 10
      BCMA2=BCMT2+BCM(3)**2
      BCMA=SQRT(BCMA2)
      CTH=BCM(3)/BCMA
      STH=0.
      CTH2=CTH**2
      IF(CTH2.LT.1.) STH=SQRT(1.-CTH2)
      BCMT=SQRT(BCMT2)
      CPH=BCM(1)/BCMT
      SPH=BCM(2)/BCMT
      NOROT=0
      GO TO 100
10    NOROT=1
100   CONTINUE
      IF(NOROT.EQ.1) GO TO 190
C-- PRELIMINARY ROTATION IN C.M. SYSTEM
      PM(4)=PIN(4)
      PM(3)=CTH*PIN(3)-STH*PIN(1)
      PM(1)=STH*PIN(3)+CTH*PIN(1)
      PM(2)=PIN(2)
C
      PM1  =CPH*PM(1)-SPH*PM(2)
      PM(2)=SPH*PM(1)+CPH*PM(2)
      PM(1)=PM1
      GO TO 200
190   DO 191 J=1,4
191   PM(J)=PIN(J)
200   CONTINUE
C-- LORENTZ BOOST PROPER
      CALL CJLORN(A,PM,POUT,1)
      RETURN
      END
