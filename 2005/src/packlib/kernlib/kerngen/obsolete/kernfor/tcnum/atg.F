*
* $Id: atg.F,v 1.1.1.1 1996/02/15 17:54:15 mclareni Exp $
*
* $Log: atg.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:15  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION ATG (RSINA,RCOSA)
C
C CERN PROGLIB# B101    ATG             .VERSION KERNFOR  4.21  890323
C ORIG. 15/11/66 JZ, re-write 8/03/89 K.S.Koelbig
C
      PARAMETER (PI = 3.14159 26535 89793)

      ATG = ATAN2(RSINA,RCOSA) + (PI-SIGN(PI,RSINA))
      RETURN
      END
