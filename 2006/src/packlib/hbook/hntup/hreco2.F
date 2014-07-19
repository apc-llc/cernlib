*
* $Id: hreco2.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hreco2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/03 26/05/93  17.11.25  by  Fons Rademakers
*-- Author :    Fons Rademakers   26/05/93
      SUBROUTINE HRECO2
*.==========>
*.
*.           Update the LNAME bank for this block.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      IOFF  = 0
      NDIM  = IQ(LBLOK+ZNDIM)
      LNAME = LQ(LBLOK-1)
*
*-- loop over all variables in this block
*
      DO 40 I = 1, NDIM
*
         ITMP = IQ(LNAME+IOFF+ZITMP)
*
         IQ(LNAME+IOFF+ZIFCON) = IQ(LTMP+ITMP+1)
         IQ(LNAME+IOFF+ZIFBIT) = IQ(LTMP+ITMP+2)
         IQ(LNAME+IOFF+ZIBANK) = 0
*
         IOFF = IOFF + ZNADDR
*
   40 CONTINUE
*
      END
