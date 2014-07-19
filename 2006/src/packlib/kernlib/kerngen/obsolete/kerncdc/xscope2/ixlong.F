*
* $Id: ixlong.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: ixlong.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IXLONG (LUN)
 
C CERN PROGLIB# Z200    IXLONG          .VERSION KERNCDC  0.1   750926
 
      LB = LXBITS(LUN)
      LW = (LB+59)/60
      IXLONG = LW
      RETURN
      END
