*
* $Id: fmtime.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmtime.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMTIME(ID,IT)
C    THIS SUBROUTINE WAS PREPARED TO GET DATE AND TIME          !
C    IN THE FORM OF 'YYMMDD'   AND  'HHMM'                      !
      INTEGER ID,IT
      CALL DATIME(ID,IT)
      FMTIME = 0
      RETURN
      END
