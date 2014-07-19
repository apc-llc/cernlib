*
* $Id: zflogc.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: zflogc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFLOGC
      CALL KUGETI(LOGL)
      CALL XZLOGL(LOGL)
      END
