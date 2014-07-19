*
* $Id: exitrc.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: exitrc.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE EXITRC (INRC)

      JRC = INRC

#if defined(CERNLIB_IBM)
      IF (JRC.GE.2)  JRC = 4
#endif
#if defined(CERNLIB_VAX)
      IF (JRC.EQ.1)  THEN
          JRC = 9
        ELSEIF (JRC.GE.2)  THEN
          JRC = 4
        ENDIF
#endif
      CALL EXITF (JRC)
      END
