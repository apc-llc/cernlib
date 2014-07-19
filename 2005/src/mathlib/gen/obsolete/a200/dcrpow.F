*
* $Id: dcrpow.F,v 1.1.1.1 1996/04/01 15:03:10 mclareni Exp $
*
* $Log: dcrpow.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:10  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCRPOW(Z1,D,Z)
      DOUBLE PRECISION D,Z1(2),Z(2),A
      IF(Z1(1) .EQ. 0.0D0 .AND. Z1(2) .EQ. 0.0D0) THEN
       Z(1)=0.0D0
       Z(2)=0.0D0
      ELSE
       A=ATAN2(Z1(2),Z1(1))
       Z(1)=0.5D0*D*LOG(Z1(1)**2+Z1(2)**2)
       Z(2)=D*A
       A=EXP(Z(1))
       Z(1)=A*COS(Z(2))
       Z(2)=A*SIN(Z(2))
      END IF
      RETURN
      END
