*
* $Id: ieor.F,v 1.1.1.1 1996/02/15 17:50:07 mclareni Exp $
*
* $Log: ieor.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION IEOR(IZV,IZW)
*
      IEOR = XOR(IZV,IZW)
*
      RETURN
      END
