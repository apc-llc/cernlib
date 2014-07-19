*
* $Id: csierr.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: csierr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 21/11/94  14.17.52  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSIERR(I)
***-----------------------
*   outputs interpreter's err. message
***-----------------------
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
      CHARACTER *8 NAME
      J=MJSCHA(NAME)
      CALL CCOPYS(MJCHAR(NAMEST(1)),J,8)
      IF(NUMST.GT.0)THEN
        WRITE(*,77)I,NAME,NUMST
      ELSE
        WRITE(*,78)I,NAME
      ENDIF
 77   FORMAT(1X,'interpreter error ',I3,2X,'routine ',A, ' line',I5)
 78   FORMAT(1X,'interpreter error ',I3,2X,'routine ',A)
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14),1-I
      GO TO 55
 1    CALL CSSOUT(' unknown command!')
      GO TO 55
 2    CALL CSSOUT(' arithmetic stack overflow')
      GO TO 55
 3    CALL CSSOUT(' character stack overflow')
      GO TO 55
 4    CALL CSSOUT(' subscript value out of bounds')
      GO TO 55
 5    CALL CSSOUT(' error in I/O statement')
      GO TO 55
 6    CALL CSSOUT(' Adjustable array dimension error:')
      CALL CSSOUT(' lower bound greater than upper bound')
      GO TO 55
 7    CALL CSSOUT(' too many arguments')
      GO TO 55
 8    CONTINUE
*     CALL CSSOUT(' missing routine')
      GO TO 55
 9    CALL CSSOUT(' exceeded recursion level')
      GO TO 55
 10   CALL CSSOUT('number of subscripts does not match declaration')
      GO TO 55
 11   CALL CSSOUT(' array subscript out of range')
      GO TO 55
 12   CALL CSSOUT(' error in open statement')
      GO TO 55
 13   CALL CSSOUT(' invalid use of intrinsic function')
      GO TO 55
 14   CALL CSSOUT(' kuip-vector was deleted')
 55   END

