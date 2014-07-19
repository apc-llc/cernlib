*
* $Id: c209s.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c209s.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C209S(Z,NT,A)
#include "iorc.inc"
C     This subroutine stores the data for test N in A
#include "gen/defc64.inc"
     +           Z(*),A(0:*)
C     Construct the polynomial coefficients
#if defined(CERNLIB_DOUBLE)
      A(0)=1.0D0
#endif
#if !defined(CERNLIB_DOUBLE)
      A(0)=1.0E0
#endif
      A(1)= -( Z(1)+Z(2)+Z(3)+Z(4) )
      A(2)= (Z(1)*Z(2))+(Z(1)*Z(3))+(Z(1)*Z(4))+
     +      (Z(2)*Z(3))+(Z(2)*Z(4))+(Z(3)*Z(4))
      A(3)= -( (Z(1)*Z(2)*Z(3))+(Z(1)*Z(2)*Z(4))+(Z(1)*Z(3)*Z(4))+
     +                          (Z(2)*Z(3)*Z(4)) )
      A(4)= Z(1)*Z(2)*Z(3)*Z(4)
      RETURN
      END
