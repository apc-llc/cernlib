#include "fatmen/pilot.h"
      SUBROUTINE FMSVER
#include "fatmen/fatveri.inc"
#include "fatmen/fatbug.inc"

      IF(IFMVER.EQ.0) THEN
         PRINT 9001
      ELSEIF(IFMVER.EQ.1) THEN
         PRINT 9002
      ELSE
         PRINT 9003
      ENDIF

9001  FORMAT(' FMSVER. verification is ON')
9002  FORMAT(' FMSVER. verification is OFF')
9003  FORMAT(' FMSVER. verification is set to unknown state')
      END
