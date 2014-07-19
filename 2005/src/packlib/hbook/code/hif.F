*
* $Id: hif.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hif.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.49  by  Rene Brun
*-- Author :
      FUNCTION HIF(IDD,I)
*.==========>
*.           RETURN FUNCTION CORRESPONDING TO CHANNEL I OF IDD
*..=========> ( R.Brun )
*.___________________________________________
      CALL HFIND(IDD,'HIF   ')
      HIF=HCX(I,3)
      END
