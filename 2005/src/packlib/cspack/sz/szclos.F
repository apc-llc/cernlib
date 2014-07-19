*
* $Id: szclos.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szclos.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZCLOS(LUN,CHOPT,IRC)
#include "cspack/czopts.inc"
      IRC = 0
      IF(IOPTD.NE.0) THEN
         CLOSE(LUN,STATUS='DELETE',ERR=99)
      ELSE
         CLOSE(LUN,STATUS = 'KEEP',ERR=99)
      ENDIF
      RETURN
99    IRC = 1
      END
