*
* $Id: rndm.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: rndm.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION RNDM (X)
C
C CERN PROGLIB# V104    RNDM            .VERSION KERNCRY  1.05  861204
C ORIG. 18/01/84  W.W. SACLAY/DPHPE
C
      RNDM = RANF ()
      RETURN
      END

      FUNCTION IRNDM (X)
      CALL RANGET(ISTOR)
      IRNDM = ISTOR
      RETURN
      END

      SUBROUTINE RDMOUT (ISEED)
      CALL RANGET(ISEED)
      RETURN
      END

      SUBROUTINE RDMIN (ISEED)
      CALL RANSET (ISEED)
      RETURN
      END
#ifdef CERNLIB_TCGEN_RNDM
#undef CERNLIB_TCGEN_RNDM
#endif
