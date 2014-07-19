*
* $Id: hdexpo.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdexpo.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 24/01/94  19.07.48  by  Rene Brun
*-- Author :
      FUNCTION HDEXPO (X)
*.==========>
*.          Evaluates function E(X)=EXP(A+B*X)
*.          and its first derivatives
*..=========> ( R.Brun, E.Lessner)
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
      DOUBLE PRECISION DD,XX
      PARAMETER (DDMAX=60.)
*.___________________________________________
      XX=X
      DD=FITPAD(1)+FITPAD(2)*XX
      IF (DD.LT.-DDMAX) DD=-DDMAX
      IF (DD.GT.DDMAX) DD=DDMAX
      FITFUN=EXP(DD)
      HDEXPO=FITFUN
      FITDER(1)=FITFUN
      FITDER(2)=FITFUN*XX
      END
 
