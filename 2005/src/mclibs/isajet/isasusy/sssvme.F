#include "isajet/pilot.h"
      SUBROUTINE SSSVME(ME)
C
C          Set MSSMOD flag for last mode in /SSMODE/
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/ssmode.inc"
C
      INTEGER ME
      MSSMOD(NSSMOD)=ME
      RETURN
      END
