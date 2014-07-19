*
* $Id: locb.F,v 1.1.1.1 1996/02/15 17:54:26 mclareni Exp $
*
* $Log: locb.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:26  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      FUNCTION LOCB (AM)
C
C CERN PROGLIB#         LOCB            .VERSION KERNDGE  1.08  891120
C ORIG. 17/11/89  JZ
C
      BYTE AM(8)
 
      LOCB= BYTEADDR (AM(1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_LOCB
#undef CERNLIB_TCGEN_LOCB
#endif
