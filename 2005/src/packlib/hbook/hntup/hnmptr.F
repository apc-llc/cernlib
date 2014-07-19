*
* $Id: hnmptr.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnmptr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 26/05/92  18.44.33  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/12/91
      INTEGER FUNCTION HNMPTR(TVAR)
*.==========>
*.
*.           Return a pointer to the first word of the variable VAR
*.           in the LNAME bank. If VAR is not found then return -1.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR
      CHARACTER*32  VAR, NAME
*
      VAR  = TVAR
      CALL CLTOU(VAR)
      LVAR = LENOCC(VAR)
      NDIM = IQ(LBLOK+ZNDIM)
      IOFF = 0
*
      DO 10 I = 1, NDIM
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         CALL CLTOU(NAME)
         IF (VAR(1:LVAR) .EQ. NAME(1:LL)) THEN
            HNMPTR = IOFF
            RETURN
         ENDIF
         IOFF = IOFF + ZNADDR
10    CONTINUE
*
      HNMPTR = -1
*
      END
