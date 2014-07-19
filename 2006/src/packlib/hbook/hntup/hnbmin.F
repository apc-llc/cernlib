*
* $Id: hnbmin.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbmin.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 18/03/93  17.10.44  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/03/93
      INTEGER FUNCTION HNBMIN()
*.==========>
*.
*.           Return the number of entries in the block with the
*.           lowest number of entries.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      LL = LQ(LCID-1)
      HNBMIN = IQ(LL+ZNOENT)
*
10    HNBMIN = MIN(HNBMIN, IQ(LL+ZNOENT))
*
      LL = LQ(LL)
      IF (LL .NE. 0) GOTO 10
*
      END
