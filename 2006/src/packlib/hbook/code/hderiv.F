*
* $Id: hderiv.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hderiv.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 17/08/89  08.50.24  by  Rene Brun
*-- Author :
      SUBROUTINE HDERIV(DERIV)
*.==========>
*.           stores values of derivatives for the parameters of the
*.           function being fitted.
*.           Routine called from the user function
*.
*..=========> ( R.Brun )
#include "hbook/hcfits.inc"
      DIMENSION DERIV(1)
*.___________________________________________
*.
      DO 10 I=1,NFPAR
         FITDER(I)=DERIV(I)
  10  CONTINUE
      END
