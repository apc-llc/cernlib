*
* $Id: lorenc.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: lorenc.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE LORENC (A,B,C)
C
      DIMENSION A(4),B(4),C(4)
C
C
      X=(A(1)*B(1)+A(2)*B(2)+A(3)*B(3))                                 -UNI
     1  /(SQRT (1.+B(1)*B(1)+B(2)*B(2)+B(3)*B(3)) +1.)                  -UNI
     2  +SQRT (1.+A(1)*A(1)+A(2)*A(2)+A(3)*A(3))                        -UNI
C
C     X1=(A(1)*B(1)+A(2)*B(2)+A(3)*B(3))                                UNI
C     X2=(SQRT (1.+B(1)*B(1)+B(2)*B(2)+B(3)*B(3)) +1.)                  UNI
C     X3=SQRT (1.+A(1)*A(1)+A(2)*A(2)+A(3)*A(3))                        UNI
C     X = X1/X2 + X3                                                    UNI
C
C
         DO 50 I=1,3
   50 C(I)=A(I)+B(I)*X
      RETURN
      END
