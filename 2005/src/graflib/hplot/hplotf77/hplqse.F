*
* $Id: hplqse.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplqse.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/06 31/10/95  11.19.41  by  O.Couet
*-- Author :    O.Couet   31/10/95
      SUBROUTINE HPLQSE(CHOPT,R)
*
* Routine used by the PAW function $GRAFINFO
*
      CHARACTER*(*) CHOPT
      CHARACTER*8   CHOPTN
*
      IF (CHOPT(1:1).EQ.'?') THEN
         CALL HPLSET (CHOPT,R)
      ELSE
         CHOPTN(1:1) = '?'
         CHOPTN(2:5) = CHOPT
         CALL HPLSET (CHOPTN(1:5),R)
      ENDIF
*
      END
