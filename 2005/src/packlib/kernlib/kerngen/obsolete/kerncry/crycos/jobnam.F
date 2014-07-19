*
* $Id: jobnam.F,v 1.1.1.1 1996/02/15 17:52:41 mclareni Exp $
*
* $Log: jobnam.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:41  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE JOBNAM (ID)
C
C CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 JZ
C
      ID = JNAME (X)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JOBNAM
#undef CERNLIB_TCGEN_JOBNAM
#endif
