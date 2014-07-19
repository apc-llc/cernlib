*
* $Id: iumode.F,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: iumode.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION IUMODE (WORD)
C
C CERN PROGLIB# M506    IUMODE          .VERSION KERNNOR  1.03  810324
C
 
      J = JBYT (WORD,25,8)
      IF (J.EQ.255)  J=0
      IUMODE = J
      RETURN
      END
#ifdef CERNLIB_TCGEN_IUMODE
#undef CERNLIB_TCGEN_IUMODE
#endif
