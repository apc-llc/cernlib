*
* $Id: htfunc.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: htfunc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 06/06/90  17.45.59  by  Rene Brun
*-- Author :
      SUBROUTINE HTFUNC(TFUNC,XHFIT,YHFIT,Z)
*.==========>
*.           COMPUTES Z=TFUNC(XHFIT,YHFIT)
*..=========> ( R.Brun )
      EXTERNAL TFUNC
*.___________________________________________
      Z=TFUNC(XHFIT,YHFIT)
  99  RETURN
      END
