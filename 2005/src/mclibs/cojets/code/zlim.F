*
* $Id: zlim.F,v 1.1.1.1 1996/01/11 14:14:45 mclareni Exp $
*
* $Log: zlim.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:45  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZLIM(PSQ,QZ1SQ,QZ2SQ,ZLW,ZUP,FLAG)
C     *********************************************
C-- CALCULATES LIMITS FOR Z - INPUT, SQUARE MASSES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      FLAG=-1.
      IF(SQRT(PSQ).LT.(SQRT(QZ1SQ)+SQRT(QZ2SQ))) GO TO 10
      B=-(1.+QZ1SQ/PSQ-QZ2SQ/PSQ)
      C=QZ1SQ/PSQ
      DISCR=B**2/4.-C
      IF(DISCR.LT.0.) GO TO 10
      SQDIS=SQRT(DISCR)
      ZLW=-B/2.-SQDIS
      ZUP=-B/2.+SQDIS
      RETURN
10    FLAG=1.
      ZLW=0.5
      ZUP=0.5
      RETURN
      END
