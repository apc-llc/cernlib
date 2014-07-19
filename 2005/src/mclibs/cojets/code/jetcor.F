*
* $Id: jetcor.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: jetcor.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE JETCOR(N)
C     ********************
C-- ROTATE AND BOOST JET SO AS TO CORRECT FOR PT UNBALANCE AND ENERGY
C-- NON-CONSERVATION
C-- CREATED: 88/04/27
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/decpar.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/par.inc"
C
      DO 1 J=1,4
1     PJTOT(J)=0.
      DO 2 I=1,N
      IF(IDB(K(I,2)).GT.0) GO TO 2
      DO 3 J=1,4
3     PJTOT(J)=PJTOT(J)+P(I,J)
2     CONTINUE
      PMTSQ=PJTOT(1)**2+PJTOT(2)**2
      PMSQ=PMTSQ+PJTOT(3)**2
      PM=SQRT(PMSQ)
      PJTOT(5)=SQRT(ABS(PJTOT(4)**2-PMSQ))
      IFLAG=0
      IF(PJTOT(5).GE.EBEG) IFLAG=1
      IF(PJTOT(1).EQ.0..AND.PJTOT(2).EQ.0.) THEN
        PHIP=0.
      ELSE
        PHIP=ATAN2X(PJTOT(2),PJTOT(1))
      ENDIF
      COSP=COS(PHIP)
      SINP=SIN(PHIP)
      IF(PM.EQ.0.) THEN
        COST=1.
        SINT=0.
      ELSE
        COST=PJTOT(3)/PM
        SINT=SQRT(PMTSQ)/PM
      ENDIF
      IF(IFLAG.EQ.0) THEN
        EXPB=(EBEG+SQRT(ABS(EBEG**2-PJTOT(5)**2)))/(PJTOT(4)+PM)
        COSHB=.5*(EXPB+1./EXPB)
        SINHB=.5*(EXPB-1./EXPB)
      ENDIF
      DO 5 I=1,N
C-- INVERSE ROTATION 1-2 AXES -- JET IN 1-3 PLANE
      P1=     P(I,1)*COSP+P(I,2)*SINP
      P(I,2)=-P(I,1)*SINP+P(I,2)*COSP
      P(I,1)=P1
C-- INVERSE ROTATION 1-3 AXES -- JET ALONG 3RD AXIS
      P3=     P(I,3)*COST+P(I,1)*SINT
      P(I,1)=-P(I,3)*SINT+P(I,1)*COST
      P(I,3)=P3
C-- BOOST ALONG 3RD AXIS -- TOTAL ENERGY = INPUT ENERGY
      IF(IFLAG.EQ.1) GO TO 5
      E=     P(I,4)*COSHB+P(I,3)*SINHB
      P(I,3)=P(I,4)*SINHB+P(I,3)*COSHB
      P(I,4)=E
5     CONTINUE
      PJTOT(1)=0.
      PJTOT(2)=0.
      IF(IFLAG.EQ.0) THEN
        PJTOT(3)=SQRT(ABS(EBEG**2-PJTOT(5)**2))
        PJTOT(4)=EBEG
      ELSE
        PJTOT(3)=PM
      ENDIF
      RETURN
      END
