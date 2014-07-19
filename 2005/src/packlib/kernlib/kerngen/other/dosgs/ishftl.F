*
* $Id: ishftl.F,v 1.1.1.1 1996/02/15 17:50:00 mclareni Exp $
*
* $Log: ishftl.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:00  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
       FUNCTION ISHFTL(IZW,NZB)
       IF (NZB .GT. 0) THEN
          ISHFTL = LSHIFT(IZW,NZB)
       ELSE IF (NZB .EQ. 0) THEN
          ISHFTL = IZW
       ELSE
          ISHFTL = ISHFT(IZW,NZB)
       END IF
       END
#endif
