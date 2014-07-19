*
* $Id: hijxy.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hijxy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HIJXY(IDD,I,J,X,Y)
*.==========>
*.           RETURNS THE VALUES I,J CORRESPONDING TO X,Y
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
*.___________________________________________
      CALL HFIND(IDD,'HIJXY ')
      DX=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      DY=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
      X=FLOAT(I-1)*DX+Q(LCID+KXMIN)
      Y=FLOAT(J-1)*DY+Q(LCID+KYMIN)
      END
