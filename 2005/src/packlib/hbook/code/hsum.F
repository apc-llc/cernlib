*
* $Id: hsum.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsum.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.52.45  by  Julian Bunn
*-- Author :
      FUNCTION HSUM(IDD)
*.==========>
*.           RETURN SUM OF CONTENTS OF IDD (NO UNDER OVERFLOWS )
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON/HCGARB/NCX,NCY,GA(15)
*.___________________________________________
      SUM=0.
      CALL HFIND(IDD,'HSUM  ')
      IF(LCID.EQ.0)GO TO 99
      NCX=IQ(LCID+KNCX)
      IF(JBIT(IQ(LCID+KBITS),1).EQ.0)GO TO 20
*
*             1-DIM HIST
*
      DO 10 I=1,NCX
         SUM=SUM+HCX(I,1)
  10  CONTINUE
      GO TO 99
*
*             2-DIM HIST
*
  20  NCY=IQ(LCID+KNCY)
      DO 40 I=1,NCX
         DO 30 J=1,NCY
            SUM=SUM+HCXY(I,J,1)
  30     CONTINUE
  40  CONTINUE
*
  99  HSUM=SUM
      END
