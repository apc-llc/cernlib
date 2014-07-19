*
* $Id: namjob.F,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: namjob.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION NAMJOB (NAM)
 
C CERN PROGLIB#         NAMJOB          .VERSION KERNCDC  0.1   760623
 
      DATA IBLANK /7H        /
 
 
      NAM    = IBLANK
      NAMJOB = IBLANK
      RETURN
      END
