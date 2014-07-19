*
* $Id: zlimsy.F,v 1.1.1.1 1996/01/11 14:14:45 mclareni Exp $
*
* $Log: zlimsy.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:45  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZLIMSY(PSQ,QZSQ,ZLW,ZUP,FLAG)
C     ****************************************
C-- CALCULATES LIMITS FOR Z WHEN SECONDARY MASSES ARE EQUAL
C-- INPUT, SQUARE MASSES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
C
      FLAG=-1.
      IF(PSQ.LT.4.*QZSQ) GO TO 10
      D=SQRT(.25-QZSQ/PSQ)
      ZLW=.5-D
      ZUP=.5+D
      RETURN
10    FLAG=1.
      ZLW=.5
      ZUP=.5
      RETURN
      END
