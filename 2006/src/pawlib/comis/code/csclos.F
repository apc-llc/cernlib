*
* $Id: csclos.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csclos.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 23/09/94  18.26.36  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   22/09/94
      SUBROUTINE CSCLOS(LUN)
      COMMON/CSLUNCS/LUNS(10)
      I=1
 1    IF(LUNS(I).EQ.LUN)THEN
         LUNS(I)=0
      ELSE
        I=I+1
        IF(I.LE.10)GO TO 1
      ENDIF
***      CLOSE(LUN)
***      print *,' info from csclos: lun=',lun
      END
