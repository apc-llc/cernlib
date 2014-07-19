*
* $Id: heleft.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: heleft.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
#if !defined(CERNLIB_DOUBLE)
      FUNCTION HELEFT (IEF,X)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HELEFT (IEF,X)
#endif
*.==========>
*.      RETURNS ELEMENTARY FUNCTION (ORDER NUMBER IEF)
*.      THIS IS A DEFAULT FUNCTION
*..=========> ( R.Brun )
      HELEFT=0.
      END
