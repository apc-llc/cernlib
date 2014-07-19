*
* $Id: hgaus.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgaus.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 28/04/93  11.20.00  by  Rene Brun
*-- Author :
      FUNCTION HGAUS (X)
*.==========>
*.           Computes value of gaussian
*.           G(X)=(COEFF/SIGM)*EXP(-0.5*((X-AVERAG)/SIGM)**2)
*.           and its first derivatives
*..=========> ( E.Lessner,D.Lienart )
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DD1,DD2,DD3
#endif
*.___________________________________________
      DD1=(X-FITPAD(2))/FITPAD(3)
      DD2=DD1*DD1
      DD3=0.
      IF (DD2.LT.172.) DD3=EXP(-0.5*DD2)
      FITFUN=FITPAD(1)*DD3
      HGAUS=FITFUN
      FITDER(1)=DD3
      FITDER(2)=FITFUN*DD1/FITPAD(3)
      FITDER(3)=FITFUN*(DD2-1.)/FITPAD(3)
      END
