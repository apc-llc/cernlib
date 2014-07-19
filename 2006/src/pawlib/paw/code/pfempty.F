*
* $Id: pfempty.F,v 1.3 1996/06/03 13:28:46 couet Exp $
*
* $Log: pfempty.F,v $
* Revision 1.3  1996/06/03 13:28:46  couet
* - add pilot.h
*
* Revision 1.1.1.1  1996/06/03 15:09:49 couet
* Paw
*
* 
#include "paw/pilot.h"
      SUBROUTINE PFEMPTY (CCTITLE)
*.==========>
*.
*.     To notify the master that an empty histogram has been produced
*.
*..=========> ( Dino FM )
*
*
#include "hbook/hcpiaf.inc"
#include "hbook/czbuff.inc"
#include "paw/pchost.inc"

      CHARACTER*(*) CCTITLE
      INTEGER ISTAT, L

      L = LENOCC (CCTITLE)
      WRITE(CHSMPF,'(A,A)') 'PEMPTY', CCTITLE(:L)
      CALL CZPUTA(CHSMPF, ISTAT)
*
      END
