*
* $Id: dclog.F,v 1.1.1.1 1996/04/01 15:03:10 mclareni Exp $
*
* $Log: dclog.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:10  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCLOG(Z,F)
      DOUBLE PRECISION Z(2),F(2),A
      A=ATAN2(Z(2),Z(1))
      F(1)=0.5D0*LOG(Z(1)**2+Z(2)**2)
      F(2)=A
      RETURN
      END
