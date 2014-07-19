*
* $Id: hmax.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmax.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.48.16  by  Julian Bunn
*-- Author :
      FUNCTION HMAX(IDD)
*.==========>
*.           RETURN MAXIMUM VALUE OF A BIN OR CELL
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      XMAX=-BIGP
      CALL HFIND(IDD,'HMAX  ')
      IF(LCID.EQ.0)GO TO 99
      NCX=IQ(LCID+KNCX)
      IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
         DO 10 I=1,NCX
            X=HCX(I,1)
            XMAX=AMAX1(XMAX,X)
  10     CONTINUE
      ELSE
         NCY=IQ(LCID+KNCY)
         DO 30 I=1,NCX
            DO 20 J=1,NCY
               X=HCXY(I,J,1)
               XMAX=AMAX1(X,XMAX)
  20        CONTINUE
  30     CONTINUE
      ENDIF
*
  99  HMAX=XMAX
      END
