*
* $Id: hpolyn.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpolyn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 28/04/93  11.20.57  by  Rene Brun
*-- Author :
      FUNCTION HPOLYN (X)
*.==========>
*.           Compute value of a polynomial and its derivatives.
*..=========> (R.Brun,E.Lessner,D.Lienart )
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DD
#endif
*.___________________________________________
*
      DD=1.
      FITDER(1)=1.
      FITFUN=FITPAD(1)
      DO 10 I=2,NFPAR
         DD=DD*X
         FITFUN=FITFUN+FITPAD(I)*DD
         FITDER(I)=DD
   10 CONTINUE
      HPOLYN=FITFUN
      END
