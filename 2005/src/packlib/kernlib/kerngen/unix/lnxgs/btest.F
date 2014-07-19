*
* $Id: btest.F,v 1.1.1.1 1996/02/15 17:50:07 mclareni Exp $
*
* $Log: btest.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      LOGICAL FUNCTION BTEST(IZW,IZP)
      BTEST = AND (IZW,LSHIFT(1,IZP)) .NE. 0
      RETURN
      END
