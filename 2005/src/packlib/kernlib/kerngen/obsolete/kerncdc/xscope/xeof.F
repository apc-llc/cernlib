*
* $Id: xeof.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: xeof.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      LOGICAL FUNCTION XEOF (LUN)
 
C CERN PROGLIB# Z200    XEOF            .VERSION KERNCDC  0.1   750926
 
      XEOF=.FALSE.
      IF (EOF (LUN)) 10,20
   10 XEOF=.TRUE.
   20 RETURN
      END
