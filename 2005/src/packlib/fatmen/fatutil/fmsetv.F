#include "fatmen/pilot.h"
      SUBROUTINE FMSETV(ICODE,IRC)
#include "fatmen/fatveri.inc"
#include "fatmen/fatbug.inc"

      IRC = 0

      IF(ICODE.NE.0.AND.ICODE.NE.1) THEN

         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) ICODE
9001  FORMAT(' FMSETV. Error - ICODE must be 0',
     +       ' or 1 (verification off/on) - ICODE = ',I6)
         
         IRC = 1
         RETURN

      ENDIF

      IFMVER = ICODE

      END
