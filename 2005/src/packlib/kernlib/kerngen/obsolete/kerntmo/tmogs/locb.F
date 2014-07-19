*
* $Id: locb.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: locb.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
       FUNCTION LOCB (VAR)
C
C CERN PROGLIB# N100    LOCB            .VERSION KERNTMO  1.02  891211
C ORIG. 07/12/89  J.CARTER
C
C     THE FUNCTION  LOCB(VAR)  RETURNS THE MACHINE ADDRESS OF 'VAR'
C
      LOCB = IADDRESS( VAR )
      RETURN
      END
#ifdef CERNLIB_TCGEN_LOCB
#undef CERNLIB_TCGEN_LOCB
#endif
