*
* $Id: hij.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hij.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.47.50  by  Julian Bunn
*-- Author :
      FUNCTION HIJ(IDD,I,J)
*.==========>
*.           RETURN THE CONTENT OF CELL (I,J) OF IDD
*..=========> ( R.Brun )
*.___________________________________________
      CALL HFIND(IDD,'HIJ   ')
      HIJ=HCXY(I,J,1)
      END
