*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
 
C CERN PROGLIB# Z203    XINBF           .VERSION KERNCDC  1.07  781218
 
      DIMENSION    XBV(NXB)
      LOGICAL      XEOF, XRDIN
 
      READ (LUN) XBV
      IF (XEOF (LUN))        GO TO 61
      IF (XRDIN(LUN))        GO TO 64
      N = IXLONG (LUN)
      N = MIN0 (N,NXB)
      NXB = MAX0 (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -7
      RETURN
      END
