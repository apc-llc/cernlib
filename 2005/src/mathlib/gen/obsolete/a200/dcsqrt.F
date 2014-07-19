*
* $Id: dcsqrt.F,v 1.1.1.1 1996/04/01 15:03:10 mclareni Exp $
*
* $Log: dcsqrt.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:10  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCSQRT(Z,F)
      DOUBLE PRECISION Z(2),F(2),A,B
      IF(Z(2) .EQ. 0.0D0) THEN
       IF(Z(1) .GE. 0.0D0) THEN
        F(1)=SQRT(Z(1))
        F(2)=0.0D0
       ELSE
        F(2)=SQRT(-Z(1))
        F(1)=0.0D0
       END IF
      ELSE
       A=SQRT(Z(1)**2+Z(2)**2)
       B=SQRT(0.5D0*(A+Z(1)))
       F(2)=SIGN(SQRT(0.5D0*(A-Z(1))),Z(2))
       F(1)=B
      END IF
      RETURN
      END
