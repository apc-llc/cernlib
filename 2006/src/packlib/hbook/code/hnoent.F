*
* $Id: hnoent.F,v 1.2 2001/09/24 14:25:12 couet Exp $
*
* $Log: hnoent.F,v $
* Revision 1.2  2001/09/24 14:25:12  couet
* - The number of entries of a non existing histogram was not 0.
*
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.50  by  Rene Brun
*-- Author :
      SUBROUTINE HNOENT(IDD,NUMB)
*.==========>
*.           RETURNS THE NUMBER OF ENTRIES OF IDD
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON /QUEST/ IQUEST(100)
*.___________________________________________
      CALL HFIND(IDD,'HNOENT')
      IF(IQUEST(1).NE.0)THEN
         NUMB=0
      ELSE
         I4=JBIT(IQ(LCID+KBITS),4)
         IF(I4.NE.0)THEN
            NUMB=IQ(LCID+3)
         ELSE
            NUMB=IQ(LCONT+KNOENT)
         ENDIF
      ENDIF
      END
