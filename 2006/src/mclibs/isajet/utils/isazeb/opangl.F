*
* $Id: opangl.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: opangl.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION OPANGL(P1,P2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DETERMINE OPENING ANGLE BETWEEN TWO VECTORS
C-
C-   Returned value  : OPENING ANGLE IN RADIANS
C-   Inputs  : P1,P2 - 4 VECTORS ( IGNORE P(4) )
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   6-NOV-1989   Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER I
      REAL DOT,COSA,OPANGL,P1(4),P2(4),P1M,P2M
C----------------------------------------------------------------------
      DOT = 0.0
      DO I = 1, 3
        DOT = DOT + P1(I)*P2(I)
      END DO
      P1M  = SQRT (P1(1)*P1(1) + P1(2)*P1(2) + P1(3)*P1(3) )
      P2M  = SQRT (P2(1)*P2(1) + P2(2)*P2(2) + P2(3)*P2(3) )
      COSA = DOT / (P1M*P2M)
      IF (ABS(COSA) .GT.1.0 )  THEN
        IF (COSA.LT.0.0) COSA = -1.0
        IF (COSA.GT.0.0) COSA =  1.0
      END IF
      OPANGL = ACOS( COSA )
  999 RETURN
      END
