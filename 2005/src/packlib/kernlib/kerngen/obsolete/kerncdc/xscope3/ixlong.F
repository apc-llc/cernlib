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
 
C CERN PROGLIB# Z200    IXLONG          .VERSION KERNCDC  1.11  800801
 
      LC  = IXPTRZL (LUN)
      LC2 = IXRLZL  (LUN)
 
C         IXPTRZL MAY RETURN LENGTH TOO LONG FOR RT=S
C         IXRLZL  RETURNS FULL RECORD LENGTH FOR RT=W
C**       THEREFOR TAKE THE MINIMUM OF THE TWO
 
      LC  = MIN0 (LC,LC2)
      LCW =(LC+9) / 10
      IXLONG = LCW
      RETURN
      END
