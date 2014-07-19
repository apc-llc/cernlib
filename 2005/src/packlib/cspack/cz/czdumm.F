*
* $Id: czdumm.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czdumm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDUMM
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_CLIENT))
      ENTRY CTL$GL_PCB
      ENTRY CTL$T_ACCOUNT
      ENTRY CTL$T_USERNAME
#endif
      END
