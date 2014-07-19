*
* $Id: csbcll.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: csbcll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if (!defined(CERNLIB_ASSEMB))&&(defined(CERNLIB_OLD))
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSBCLL
     1    (NAME,N,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      EXTERNAL NAME
      IF (N.EQ.0)THEN
      CALL NAME
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10),N
 1    CALL NAME(P1)
      GO TO 99
 2    CALL NAME(P1,P2)
      GO TO 99
 3    CALL NAME(P1,P2,P3)
      GO TO 99
 4    CALL NAME(P1,P2,P3,P4)
      GO TO 99
 5    CALL NAME(P1,P2,P3,P4,P5)
      GO TO 99
 6    CALL NAME(P1,P2,P3,P4,P5,P6)
      GO TO 99
 7    CALL NAME(P1,P2,P3,P4,P5,P6,P7)
      GO TO 99
 8    CALL NAME(P1,P2,P3,P4,P5,P6,P7,P8)
      GO TO 99
 9    CALL NAME(P1,P2,P3,P4,P5,P6,P7,P8,P9)
      GO TO 99
 10   CALL NAME(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
 99   CONTINUE
      ENDIF
      END
#endif
