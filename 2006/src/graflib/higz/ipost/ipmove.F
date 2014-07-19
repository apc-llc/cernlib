*
* $Id: ipmove.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: ipmove.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 26/09/94  15.23.41  by  O.Couet
*-- Author :    O.Couet   05/04/94
      SUBROUTINE IPMOVE(IX,IY)
      IF (IX.NE.0.AND.IY.NE.0) THEN
          CALL IPIOUT(IX)
          CALL IPIOUT(IY)
          CALL IPPSTF(2,' d')
       ELSEIF (IX.NE.0) THEN
          CALL IPIOUT(IX)
          CALL IPPSTF(2,' X')
       ELSEIF (IY.NE.0) THEN
          CALL IPIOUT(IY)
          CALL IPPSTF(2,' Y')
       ENDIF
       END
