*
* $Id: c206s1.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c206s1.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C206S1(Z,NT,A)
#include "iorc.inc"
C     This subroutine stores the data for test N in A
      COMPLEX Z(*),A(*)
C     Construct the polynomial coefficients
      A(1)=1.0
      A(2)= -( Z(1)+Z(2)+Z(3)+Z(4) )
      A(3)= (Z(1)*Z(2))+(Z(1)*Z(3))+(Z(1)*Z(4))+
     +      (Z(2)*Z(3))+(Z(2)*Z(4))+(Z(3)*Z(4))
      A(4)= -( (Z(1)*Z(2)*Z(3))+(Z(1)*Z(2)*Z(4))+(Z(1)*Z(3)*Z(4))+
     +                          (Z(2)*Z(3)*Z(4)) )
      A(5)= Z(1)*Z(2)*Z(3)*Z(4)
      RETURN
      END
