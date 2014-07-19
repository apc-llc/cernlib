#include "cojets/pilot.h"

      PROGRAM COJETST
      CHARACTER*(*) DFILE
#if !defined(CERNLIB_GFORTRAN)
      EXTERNAL COJDAT
#else
      EXTERNAL COJDAT, COJVER
#endif
#if defined(CERNLIB_IBMVM)
      PARAMETER (DFILE='/COJETS DATA *')
#else
      PARAMETER (DFILE='cojets.dat')
#endif
#if defined(CERNLIB_GFORTRAN)
      call dummysub(cojdat)
      call dummysub(cojver)
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
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif
