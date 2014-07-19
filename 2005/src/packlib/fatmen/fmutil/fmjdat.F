*
* $Id: fmjdat.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmjdat.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMJDAT(IYEAR,IMONTH,IDAY)
*
*     Return Julian date
*
      DIMENSION IDAYS(12)
      DATA      IDAYS/00,31,59,90,120,151,181,212,243,273,304,334/
 
      JYEAR  = MOD(IYEAR,100)
      FMJDAT = JYEAR * 10000 + IDAYS(IMONTH) + IDAY
 
      IF((JYEAR/4)*4.EQ.JYEAR.AND.IMONTH.GT.2) FMJDAT = FMJDAT + 1
 
      END
