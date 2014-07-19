*
* $Id: hbpro.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbpro.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.46  by  Rene Brun
*-- Author :
      SUBROUTINE HBPRO (ID2,VALMAX)
*.==========>
*.           BOOKS PROJECTION X AND Y
*..=========> ( R.Brun )
*.___________________________________________
      NARG   = 2
      CALL NOARG (NARG)
      VALMA = 0.
      IF (NARG .EQ. 2)          VALMA = FLOARG (VALMAX)
*
      CALL HBPROX (ID2,VALMA)
      CALL HBPROY (ID2,VALMA)
      END
