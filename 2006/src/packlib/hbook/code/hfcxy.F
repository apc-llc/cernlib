*
* $Id: hfcxy.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfcxy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 17/08/94  12.08.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFCXY(ICX,ICY,X)
*.==========>
*.           FILLS THE CELL DEFINED BY ICX AND ICY WITH X
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      NW=32/NB
      J=(IQ(LCID+KNCY)-ICY+1)*(IQ(LCID+KNCX)+2)
      L2=ICX+J
      L1=L2/NW +KCON2
      IF(NW.NE.1)THEN
         L2=(NW-1-MOD(L2,NW))*NB +1
         IX=X
         CALL SBYT(IX,IQ(LCONT+L1),L2,NB)
      ELSE
         Q(LCONT+L1)=X
      ENDIF
      AX=ABS(X)
      IF(AX.GT.1.E8.OR.X.LT.0..OR.MOD(AX,1.).GT.1.E-5)THEN
         IQ(LCONT+KNOENT)=-1
      ELSE
         IF(IQ(LCONT+KNOENT).GE.0)THEN
            IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+INT(X)
         ENDIF
      ENDIF
      END
