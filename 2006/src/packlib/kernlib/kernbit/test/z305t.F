*
* $Id: z305t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: z305t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_IBMVM)
      SUBROUTINE Z305T
      CHARACTER*40 STRING
      DATA STRING/'NULL LINE'/
      CALL KBHDR('Z305 VMSTAK')
      CALL VMSTAK(STRING,'L',IRC)
      PRINT 1000,IRC,STRING
1000  FORMAT(6X,'RC=',I3,' after putting on the stack: ',A)
      RETURN
      END
#endif
