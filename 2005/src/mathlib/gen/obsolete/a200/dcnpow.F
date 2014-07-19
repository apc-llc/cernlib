*
* $Id: dcnpow.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dcnpow.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCNPOW(Z1,N,Z)
      DOUBLE PRECISION Z1(2),Z(2),A,P(2),C(2)
      C(1)=1.0D0
      C(2)=0.0D0
      IF(Z1(1) .EQ. 0.0D0 .AND. Z1(2) .EQ. 0.0D0) THEN
       C(1)=0.0D0
      ELSE
       NS=ABS(N)
       P(1)=Z1(1)
       P(2)=Z1(2)
       GO TO 2
    1  A=P(1)*P(2)
       P(1)=(P(1)+P(2))*(P(1)-P(2))
       P(2)=A+A
    2  NP=NS
       NS=NS/2
       IF(2*NS .NE. NP) THEN
        A=P(1)*C(1)-P(2)*C(2)
        C(2)=P(2)*C(1)+P(1)*C(2)
        C(1)=A
       END IF
       IF(NS. NE. 0) GO TO 1
       IF(N .LT. 0) THEN
        A=1.0D0/(C(1)**2+C(2)**2)
        C(1)=C(1)*A
        C(2)=-C(2)*A
       END IF
      END IF
      Z(1)=C(1)
      Z(2)=C(2)
      RETURN
      END
