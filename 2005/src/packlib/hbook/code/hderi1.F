*
* $Id: hderi1.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hderi1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 19/06/89  10.00.53  by  Rene Brun
*-- Author :
      SUBROUTINE HDERI1(IDD,X,PARAM,DERIV)
*.==========>
*.            FOR EACH CASE ,EXCEPT 2-DIM.HISTOGRAM,SEE HDERI2
*.           sets values of derivatives for the parameters of the
*.           external function ufunc (see hfit note)
*.
*.           INPUT     IDD     = HISTOGRAM IDENTIFIER
*.           ARGUMENTS
*.                     X       = ARGUMENTS OF FUNCTION: CENTRE OF
*.                                CHANNEL OR CELL (ARRAY)
*.
*.                     PARAM   = CURRENT VALUES OF THE PARAMETERS
*.                                       (ARRAY)
*.
*.           OUTPUT    DERIV   =DERIVATIVES (ARRAY)
*.           ARGUMENT
*..=========> ( I.Ivanchenko )
#if !defined(CERNLIB_DOUBLE)
      DIMENSION X(1),PARAM(1),DERIV(1)
#endif
#if defined(CERNLIB_DOUBLE)
      DIMENSION X(1)
      DOUBLE PRECISION PARAM(1),DERIV(1)
#endif
*.___________________________________________
*     IF(IDD.NE.2)THEN
*        DERIV(1)=EXP(PARAM(1)+PARAM(2)*X(1))
*        DERIV(2)=DERIV(1)*X(1)
*     ELSE
*        R1GAUS=(X(1)-PARAM(2))/PARAM(3)
*        R2GAUS=R1GAUS**2
*        DERIV(1)=EXP(-0.5*R2GAUS)
*        HGAUSS=PARAM(1)*DERIV(1)
*        DERIV(2)=HGAUSS*R1GAUS/PARAM(3)
*        DERIV(3)=HGAUSS*R2GAUS/PARAM(3)
*     ENDIF
      END
