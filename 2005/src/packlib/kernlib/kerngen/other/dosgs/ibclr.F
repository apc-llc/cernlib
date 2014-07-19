*
* $Id: ibclr.F,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
*
* $Log: ibclr.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
      FUNCTION IBCLR(IZW,IZP)
      IBCLR = AND (IZW,NOT(LSHIFT(1,IZP)))
      RETURN
      END
#endif
