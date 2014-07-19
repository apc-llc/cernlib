*
* $Id: hselbf.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hselbf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.53  by  Rene Brun
*-- Author :
      LOGICAL FUNCTION HSELBF (IELEF)
*.==========>
*.      SELECTS OR REJECTS BASIC FUNCTION IELEF
*.      THIS IS A DEFAULT FUNCTION
*..=========> ( R.Brun )
      DIMENSION IELEF(1)
      HSELBF=.TRUE.
      END
