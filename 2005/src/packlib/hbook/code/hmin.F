*
* $Id: hmin.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmin.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.48.40  by  Julian Bunn
*-- Author :
      FUNCTION HMIN(IDD)
*.==========>
*.           RETURN MINIMUM VALUE OF A BIN OR CELL
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      XMIN=BIGP
      CALL HFIND(IDD,'HMIN  ')
      IF(LCID.EQ.0)GO TO 99
*
      NCX=IQ(LCID+KNCX)
      IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
         DO 10 I=1,NCX
            X=HCX(I,1)
   10    XMIN=MIN(XMIN,X)
      ELSE
         NCY=IQ(LCID+KNCY)
         DO 30 I=1,NCX
            DO 20 J=1,NCY
               X=HCXY(I,J,1)
               XMIN=MIN(XMIN,X)
   20       CONTINUE
   30    CONTINUE
      ENDIF
*
   99 HMIN=XMIN
      END
