*
* $Id: hxyij.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hxyij.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HXYIJ(IDD,X,Y,I,J)
*.==========>
*.           RETURNS THE VALUE OF I,J CORRESPONDING TO X,Y
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
*.___________________________________________
      CALL HFIND(IDD,'HXYIJ ')
      DX=FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
      DY=FLOAT(IQ(LCID+KNCY))/(Q(LCID+KYMAX)-Q(LCID+KYMIN))
      I=(X-Q(LCID+KXMIN))*DX+1.
      J=(Y-Q(LCID+KYMIN))*DY+1.
      IF(X.LT.Q(LCID+KXMIN))I=0
      IF(X.GT.Q(LCID+KXMAX))I=IQ(LCID+KNCX)+1
      IF(Y.LT.Q(LCID+KYMIN))J=0
      IF(Y.GT.Q(LCID+KYMAX))J=IQ(LCID+KNCY)+1
      END
