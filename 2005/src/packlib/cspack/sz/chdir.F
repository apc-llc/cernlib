*
* $Id: chdir.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: chdir.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if (defined(CERNLIB_IBMRT))&&(defined(CERNLIB_PLODGE))
      INTEGER FUNCTION CHDIR_(CHPATH)
      CHARACTER*()  CHDIR_
      CHARACTER*255 PATH
      LOGICAL*1     IPATH(255)
      EQUIVALENCE   (IPATH(1),PATH)
      PATH   = CHPATH
*
*     Null terminate PATH string for call to C RTL
*
      PATH(LENOCC_(CHPATH)+1) = .FALSE.
      CHDIR_ = CHDIR(PATH)
      END
 
#endif
