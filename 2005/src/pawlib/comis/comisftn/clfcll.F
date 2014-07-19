*
* $Id: clfcll.F,v 1.1.1.1 1996/02/26 17:16:51 mclareni Exp $
*
* $Log: clfcll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:51  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if (!defined(CERNLIB_ASSEMB))&&(defined(CERNLIB_OLD))
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION CLFCLL(NAME,N,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      LOGICAL CLFCLL,NAME
      EXTERNAL NAME
      IF (N.EQ.0)THEN
      CLFCLL=NAME()
      ELSE
      GO TO (1,2,3,4,5,6,7,8,9,10),N
 1    CLFCLL=NAME(P1)
      GO TO 99
 2    CLFCLL=NAME(P1,P2)
      GO TO 99
 3    CLFCLL=NAME(P1,P2,P3)
      GO TO 99
 4    CLFCLL=NAME(P1,P2,P3,P4)
      GO TO 99
 5    CLFCLL=NAME(P1,P2,P3,P4,P5)
      GO TO 99
 6    CLFCLL=NAME(P1,P2,P3,P4,P5,P6)
      GO TO 99
 7    CLFCLL=NAME(P1,P2,P3,P4,P5,P6,P7)
      GO TO 99
 8    CLFCLL=NAME(P1,P2,P3,P4,P5,P6,P7,P8)
      GO TO 99
 9    CLFCLL=NAME(P1,P2,P3,P4,P5,P6,P7,P8,P9)
      GO TO 99
 10   CLFCLL=NAME(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
 99   CONTINUE
      ENDIF
      END
#endif
