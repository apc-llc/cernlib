*
* $Id: abend.F,v 1.1.1.1 1996/02/15 17:52:41 mclareni Exp $
*
* $Log: abend.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:41  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE ABEND
C
C CERN PROGLIB# Z035    ABEND           .VERSION KERNCRY  1.01  830712
C ORIG. 12/06/83 JZ
C

      CALL SETRPV (0,0,0)
      CALL ABORT ('***** ABEND.')
      END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif
