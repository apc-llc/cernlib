*
* $Id: ischar.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: ischar.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ISCHAR ( ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gives the charge of the particle , depending
C-                         on the ISAJET identifier ID.
C-
C-   Inputs  : ID     = Isajet particle ident
C-   Outputs : ISCHAR = 0 Neutral particle
C-                    = +-1 positive or negative charged particle
C-
C-   Created  14-APR-1988   Ghita Rahal-Callot
C-   Modified to use ISAJET charge, Serban Protopopescu 30-Nov-1988
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER ID,  ISCHAR
      REAL CHARGE
C----------------------------------------------------------------------
      ISCHAR=INT(CHARGE(ID))
  999 RETURN
      END
