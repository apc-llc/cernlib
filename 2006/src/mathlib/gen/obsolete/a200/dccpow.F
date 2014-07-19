*
* $Id: dccpow.F,v 1.1.1.1 1996/04/01 15:03:10 mclareni Exp $
*
* $Log: dccpow.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:10  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCCPOW(Z1,Z2,Z)
      DOUBLE PRECISION Z1(2),Z2(2),Z(2),A,Z0(2)
      IF(Z1(1) .EQ. 0.0D0 .AND. Z1(2) .EQ. 0.0D0) THEN
       Z(1)=0.0D0
       Z(2)=0.0D0
      ELSE
       A=ATAN2(Z1(2),Z1(1))
       Z0(1)=0.5D0*LOG(Z1(1)**2+Z1(2)**2)
       Z0(2)=A
       A=Z0(1)*Z2(1)-Z0(2)*Z2(2)
       Z(2)=Z0(2)*Z2(1)+Z0(1)*Z2(2)
       Z(1)=A
       A=EXP(Z(1))
       Z(1)=A*COS(Z(2))
       Z(2)=A*SIN(Z(2))
      END IF
      RETURN
      END
