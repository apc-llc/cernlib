*
* $Id: ibset.F,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
*
* $Log: ibset.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
      FUNCTION IBSET(IZW,IZP)
      IBSET =  OR (IZW,LSHIFT(1,IZP))
      RETURN
      END
#endif
