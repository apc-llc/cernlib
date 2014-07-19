*
* $Id: hgauss.F,v 1.1.1.1 1996/01/16 17:08:12 mclareni Exp $
*
* $Log: hgauss.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.49  by  Rene Brun
*-- Author :
      FUNCTION HGAUSS(ARGUM)
*.==========>
*.           CALCULATES VALUE OF THE GAUSSIAN
*.           G(X)=COEFF*EXP(-0.5*((X-AVERAG)/SIGMA)**2)
*.           AND ITS DERIVATIVES ON COEFF,AVERAG,SIGMA
*..=========> ( I.Ivanchenko )
#include "hcfit3.inc"
#include "hcbook.inc"
      COMMON/HCGARB/R1GAUS,R2GAUS,GA(15)
*.___________________________________________
      R1GAUS=(ARGUM-Q(IPARAM+1))/Q(IPARAM+2)
      R2GAUS=R1GAUS**2
      Q(IDERIV)=0.
      IF(R2GAUS.LT.100.)Q(IDERIV)=EXP(-0.5*R2GAUS)
      HGAUSS=Q(IPARAM)*Q(IDERIV)
      Q(IDERIV+1)=HGAUSS*R1GAUS/Q(IPARAM+2)
      Q(IDERIV+2)=HGAUSS*R2GAUS/Q(IPARAM+2)
      END
