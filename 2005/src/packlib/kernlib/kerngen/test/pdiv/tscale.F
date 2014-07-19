*
* $Id: tscale.F,v 1.1.1.1 1996/02/15 17:54:56 mclareni Exp $
*
* $Log: tscale.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:56  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TSCALE

#include "kerngent/mkcde.inc"

      DIMENSION    CB(10),IC(10),IN(10)
      EQUIVALENCE (CB(1),A(11)),(IC(1),A(21)),(IN(1),B(11))

      CALL NEWGUY ('PSCALE.','TSCALE  ')
      ZERLEV = 2*ZEROV(2)

      A(1)=1234567800.
         DO 5 I=2,8
    5 A(I)=A(I-1)*.001

      A(9)=0.

C--                PREPARE CHECK-VECTOR

      IC(1) = -6
      CB(1) = 10.**(-6)

         DO 10 I=2,6
      IC(I) = IC(I-1) + 3
   10 CB(I) = CB(I-1) * 1000.

      IC(7)=IC(6)
      IC(8)=IC(6)
      IC(9)=IC(6)
      CB(7)=CB(6)
      CB(8)=CB(6)
      CB(9)=CB(6)

         DO 20 I=1,9
   20 B(I)=PSCALE (IN(I),9,A(I),4)

      CALL MVERIF (1,B,CB,9)
      CALL MVERII (2,IN,IC,9)
      RETURN
      END
