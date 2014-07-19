*
* $Id: z100t.F,v 1.2 1997/02/04 17:34:01 mclareni Exp $
*
* $Log: z100t.F,v $
* Revision 1.2  1997/02/04 17:34:01  mclareni
* Merge Winnt and 97a versions
*
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE Z100T
      INTEGER NAME(2)
      CALL KBHDR('Z100 JOBNAM')
      CALL JOBNAM(NAME)
      PRINT 1000,NAME
1000  FORMAT(6X,'Batch process name: ',2A4)
      RETURN
      END
