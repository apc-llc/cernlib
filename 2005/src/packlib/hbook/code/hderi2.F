*
* $Id: hderi2.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hderi2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 19/06/89  10.01.34  by  Rene Brun
*-- Author :
      SUBROUTINE HDERI2(IDD,X,Y,PARAM,DERIV)
*.==========>
*.           sets values of derivatives for the parameters of the
*.           external function ufunc (see hfit note)
*.
*.           INPUT     IDD     = HISTOGRAM IDENTIFIER
*.           ARGUMENTS
*.                     X       =
*.                               ARGUMENTS OF FUNCTION:CENTRE OF CELL
*.                     Y       =
*.
*.                     PARAM   = CURRENT VALUES OF THE PARAMETERS
*.                                       (ARRAY)
*.           OUTPUT    DERIV   = DERIVATIVES (ARRAY)
*.           ARGUMENT
*..=========> ( I.Ivanchenko )
#if !defined(CERNLIB_DOUBLE)
      DIMENSION PARAM(1),DERIV(1)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PARAM(1),DERIV(1)
#endif
*.___________________________________________
      END
