*
* $Id: hx.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hx.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      FUNCTION HX(IDD,X)
*.==========>
*.           RETURN THE CONTENT OF CHANNEL CORRESPONDING TO ABSCISSA X
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      CALL HFIND(IDD,'HX    ')
      CALL HDCOFL
      IF(I6.EQ.0)THEN
         DX=FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
         I=(X-Q(LCID+KXMIN))*DX+1.
      ELSE
         LBINS=LQ(LCID-2)
         DO 10 IC=1,IQ(LCID+KNCX)
            IF(X.GE.Q(LBINS+IC))I=IC
  10     CONTINUE
      ENDIF
      IF(X.LT.Q(LCID+KXMIN))I=0
      IF(X.GT.Q(LCID+KXMAX))I=IQ(LCID+KNCX)+1
      HX=HCX(I,1)
      END
