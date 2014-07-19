*
* $Id: hpkr32.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpkr32.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 16/06/89  15.20.45  by  Rene Brun
*-- Author :
      FUNCTION HPKR32(IVALUE)
*.==========>
*.       Convert real number from machine independent format
*..=========> ( R.Brun )
*.___________________________________________
#include "hbook/jbyt.inc"
      PACK32 = 0.0
      IF(IVALUE.EQ.0) GO TO 99
      XMANT = JBYT(IVALUE,1,24)
      INTEXP = 68 - JBYT(IVALUE,25,7)
      IF(IABS(INTEXP).GT.24)GO TO 99
      PACK32 = XMANT / (16.0**INTEXP)
      IF(JBIT(IVALUE,32).EQ.1) PACK32 = -PACK32
99    HPKR32 = PACK32
      RETURN
      END
