#include "fatmen/pilot.h"
      SUBROUTINE FMKVER
#include "fatmen/fatveri.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*80  OPTION
      PARAMETER     (IZERO=0)
      PARAMETER     (IONE=1)

      CALL KUGETC(OPTION,LOPT)

      IF(LOPT.LE.0) RETURN

      IF(OPTION(1:LOPT).EQ.'OFF') THEN
         PRINT *,'FMKVER. setting verification off'
         CALL FMSETV(IZERO,IRC)
      ELSEIF(OPTION(1:LOPT).EQ.'ON') THEN
         PRINT *,'FMKVER. setting verification on'
         CALL FMSETV(IONE,IRC)
      ELSE
         PRINT 9001,OPTION(1:LOPT)
9001  FORMAT(' FMKVER. error - option must be OFF or ON (not ',A,')')
      ENDIF

      END
