*
* $Id: lotran.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: lotran.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE LOTRAN (A,B,C)
C
      DIMENSION A(4),B(4),C(4)
C
C
      AMASS=SQRT (B(4)*B(4)-B(1)*B(1)-B(2)*B(2)-B(3)*B(3))
      X=((A(1)*B(1)+A(2)*B(2)+A(3)*B(3)) /(B(4)+AMASS)
     1   +SQRT (A(1)*A(1)+A(2)*A(2)+A(3)*A(3)+A(4)*A(4))) /AMASS
      C(4)= A(4)
C
         DO 50 I=1,3
   50 C(I)=A(I)+B(I)*X
      RETURN
      END
