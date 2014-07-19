*
* $Id: iumode.F,v 1.1.1.1 1996/02/15 17:54:26 mclareni Exp $
*
* $Log: iumode.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:26  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      FUNCTION IUMODE (IWORD)
C
C CERN PROGLIB# M506    IUMODE          .VERSION KERNDGE  1.01  860217
C ORIG. 03/02/86  JZ
C
C
      J = ISHFT (IWORD,-24)
      IF (J.EQ. 255) J=0
C
      IUMODE= J
      RETURN
      END
#ifdef CERNLIB_TCGEN_IUMODE
#undef CERNLIB_TCGEN_IUMODE
#endif
