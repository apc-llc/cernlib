*
* $Id: papint.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: papint.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION PAPINT(IFUN,Z)
C     ***********************
C-- INDEFINITE INTEGRALS OF SPLITTING PROBABILITY FUNCTIONS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DATA ICALL/0/
      IF(ICALL.GT.0) GO TO 10
      ICALL=1
      COLORS=3.
      CA=COLORS
      CF=(COLORS**2-1.)/(2.*COLORS)
10    CONTINUE
C
      IF(IFUN.EQ.1) GO TO 1
      IF(IFUN.EQ.2) GO TO 2
      IF(IFUN.EQ.3) GO TO 3
C
C-- Q --+ Q + G
1     CONTINUE
      Z1=1.-Z
      PAPINT=-(LOG(Z1)+Z1*(-1.+0.25*Z1))
      PAPINT=2.*CF*PAPINT
      RETURN
C
C-- G --+ G + G
2     CONTINUE
      PAPINT=-LOG(1.-Z)+LOG(Z)+Z*(-2.+Z*(0.5-Z/3.))
      PAPINT=CA*PAPINT
      RETURN
C
C-- G --+ Q + QB
3     CONTINUE
      PAPINT=(Z**3-(1.-Z)**3)/6.
      RETURN
C
      END
