#include "isajet/pilot.h"
#if defined(CERNLIB_RANLUX)
      REAL FUNCTION RANF()
C
C          Call RANLUX instead of 48-bit congruental generator RANF
C          Dummy RANFGT/RANFST/RANFMT
C
      DIMENSION X(1)
      CALL RANLUX(X,1)
      RANF=X(1)
      RETURN
      END
C
      SUBROUTINE RANFGT(SEED)
      RETURN
      END
      SUBROUTINE RANFST(SEED)
      DOUBLE PRECISION SEED
      RETURN
      END
      SUBROUTINE RANFMT
      RETURN
      END
#endif
