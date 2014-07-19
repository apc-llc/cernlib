*
* $Id: z265t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: z265t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE Z265T
      REAL*8 D,T1,T2
      DATA ILAPSE/10/
      CALL KBHDR('Z265 SLEEPF')
      CALL DATIMH(D,T1)
      PRINT 1000,D,T1,ILAPSE
      CALL SLEEPF(ILAPSE)
      CALL DATIMH(D,T2)
      PRINT 1000,D,T2,ILAPSE
1000  FORMAT(6X,'Current date/time: ',2A10,' sleeping',I5,' sec')
      RETURN
      END
