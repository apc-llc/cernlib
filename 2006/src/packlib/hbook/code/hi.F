*
* $Id: hi.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hi.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.49  by  Rene Brun
*-- Author :
      FUNCTION HI(IDD,I)
*.==========>
*.           RETURNS THE CONTENT OF CHANNEL I OF IDD
*..=========> ( R.Brun )
*.___________________________________________
      CALL HFIND(IDD,'HI    ')
      HI=HCX(I,1)
      END
