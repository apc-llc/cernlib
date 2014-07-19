*
* $Id: movlev.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: movlev.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE MOVLEV(A,B,N)
C     ************************
C-- ADAPTED FROM ISAJET
C          MOVE N CONSECUTIVE LOCATIONS FROM A TO B.
C          CAN BE REPLACED BY UCOPY FROM CERN LIBRARY.
C-- CREATED: 88/05/08
#include "cojets/itapes.inc"
      DIMENSION A(N),B(N)
      DO 100 I=1,N
      B(I)=A(I)
100   CONTINUE
      RETURN
      END
