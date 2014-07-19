*
* $Id: f106s2.F,v 1.1.1.1 1996/04/01 15:01:27 mclareni Exp $
*
* $Log: f106s2.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:27  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE F106S2(N,A,B,C)
#include "iorc.inc"
C     Subroutine to multiply together two nxn matrices, result in C
      REAL A(N,N),B(N,N),C(N,N)
      DO 2200 I=1,N
         DO 2100 J=1,N
            SUM=A(I,1)*B(1,J)
            DO 2000 K=2,N
               SUM=SUM+(A(I,K)*B(K,J))
2000        CONTINUE
            C(I,J)=SUM
2100     CONTINUE
2200  CONTINUE
      RETURN
      END
