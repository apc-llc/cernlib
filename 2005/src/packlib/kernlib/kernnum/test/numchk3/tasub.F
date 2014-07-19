*
* $Id: tasub.F,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: tasub.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE          RASUB(IDIM,X,Y,Z)
          REAL                X(*), Y(*), Z(*)
#include "asub.inc"
          SUBROUTINE          DASUB(IDIM,X,Y,Z)
          DOUBLE PRECISION    X(*), Y(*), Z(*)
#include "asub.inc"
          SUBROUTINE          CASUB(IDIM,X,Y,Z)
          COMPLEX             X(*), Y(*), Z(*)
#include "asub.inc"
          REAL FUNCTION       RASUM(IDIM,X)
          REAL                X(*)
          RASUM  =  0.
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =  1, IDIM
             RASUM  =  RASUM + X(J)
  10         CONTINUE
          RETURN
          END
          DOUBLE PRECISION FUNCTION DASUM(IDIM,X)
          DOUBLE PRECISION X(*)
          DASUM  =  0.D0
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =  1, IDIM
             DASUM  =  DASUM + X(J)
  10         CONTINUE
          RETURN
          END
          COMPLEX FUNCTION    CASUM(IDIM,X)
          COMPLEX             X(*)
          CASUM  =  (0.,0.)
          IF(IDIM .LE. 0)  RETURN
          DO 10     J  =  1, IDIM
             CASUM = CASUM + X(J)
  10         CONTINUE
          RETURN
          END
