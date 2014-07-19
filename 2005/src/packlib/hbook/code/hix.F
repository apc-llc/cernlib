*
* $Id: hix.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hix.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HIX(IDD,I,X)
*.==========>
*.           RETURNS THE ABSCISSA CORRESPONDING TO CHANNEL I
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      CALL HFIND(IDD,'HIX   ')
      CALL HDCOFL
      IF(I6.EQ.0)THEN
         DX=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
         X=FLOAT(I-1)*DX+Q(LCID+KXMIN)
      ELSE
         LBINS=LQ(LCID-2)
         X=Q(LBINS+I)
      ENDIF
      END
