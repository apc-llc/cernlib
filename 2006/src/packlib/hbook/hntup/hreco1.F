*
* $Id: hreco1.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hreco1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/03 26/05/93  16.09.31  by  Fons Rademakers
*-- Author :    Fons Rademakers   26/05/93
      SUBROUTINE HRECO1(ID1)
*.==========>
*.
*.           Sets the receiving addresses for index variables in
*.           the current block. To be able to recover an Ntuple
*.           we must read the index variables so we can calculate
*.           the number of elements to be recovered of array columns
*.           depending on the index variables.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcntpaw.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*8   BLOCK
      CHARACTER*32  VAR
      LOGICAL       INDVAR
*
      INDCR4 = 1
*
      IOFF  = 0
      NDIM  = IQ(LBLOK+ZNDIM)
      LNAME = LQ(LBLOK-1)
      CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLOCK, 8)
*
*-- loop over all variables in this block
*
      DO 40 I = 1, NDIM
         INDVAR = .FALSE.
         IF (JBIT(IQ(LNAME+IOFF+ZDESC),28) .EQ. 1) INDVAR = .TRUE.
*
         IF (INDVAR) THEN
            LL = IQ(LNAME+IOFF+ZLNAME)
            LV = IQ(LNAME+IOFF+ZNAME)
            VAR = ' '
            CALL UHTOC(IQ(LCHAR+LV), 4, VAR, LL)
            CALL HBNAME(ID1, BLOCK, RVAR4(INDCR4), '$SET:'//VAR)
            INDCR4 = INDCR4 + 1
         ENDIF
*
         IOFF = IOFF + ZNADDR
*
   40 CONTINUE
*
      END
