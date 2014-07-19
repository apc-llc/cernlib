*
* $Id: irndm.F,v 1.1 2006/09/15 09:35:25 mclareni Exp $
*
* $Log: irndm.F,v $
* Revision 1.1  2006/09/15 09:35:25  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
        FUNCTION IRNDM(ISEEK)
        COMMON /MCGN/ MCGN

C--       Integer random number
      MCGN  = MCGN * 69069
      IRNDM = ISHFT (MCGN,-1)
      RETURN
        END
