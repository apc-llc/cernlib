*
* $Id: jobnam.F,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: jobnam.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JOBNAM (NAM)
 
C CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCDC  1.12  801111
 
      DATA IBLANK /7H        /
 
 
      NAM    = IBLANK
      JOBNAM = IBLANK
      RETURN
      END
