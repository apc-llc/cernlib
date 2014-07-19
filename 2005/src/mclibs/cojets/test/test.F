#include "cojets/pilot.h"

      PROGRAM COJETST
      CHARACTER*(*) DFILE
      EXTERNAL COJDAT
#if defined(CERNLIB_IBMVM)
      PARAMETER (DFILE='/COJETS DATA *')
#else
      PARAMETER (DFILE='cojets.dat')
#endif
C
C          MAIN PROGRAM FOR COJETS
C
      OPEN(1,FILE=DFILE,STATUS='OLD')
#if defined(CERNLIB_IBMVM)
      OPEN(2,STATUS='SCRATCH',FORM='UNFORMATTED')
#endif
#if 0 
    Note that there is nothing for CERNLIB_VAXVMS!
#endif
#if !defined(CERNLIB_IBMVM) && !defined(CERNLIB_VAXVMS)
      OPEN(2,FILE='/dev/null',STATUS='UNKNOWN',FORM='UNFORMATTED')
#endif
C
      CALL COJETS(-1,-2,5,6)
C
      END
