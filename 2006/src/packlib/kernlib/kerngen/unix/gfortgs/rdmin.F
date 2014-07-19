*
* $Id: rdmin.F,v 1.1 2006/09/15 09:35:26 mclareni Exp $
*
* $Log: rdmin.F,v $
* Revision 1.1  2006/09/15 09:35:26  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
        SUBROUTINE RDMIN(ISEED)
        COMMON /MCGN/ MCGN

C--       Set the seed
      MCGN = ISEED
      RETURN

C--       Get the seed
      ENTRY RDMOUT (ISEED)
      ISEED = MCGN
      END
