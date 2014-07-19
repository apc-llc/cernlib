*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
 
C CERN PROGLIB# Z203    XINBS           .VERSION KERNCDC  2.02  820818
 
      DIMENSION    XBV(NXB), XAV(NA)
      LOGICAL      XEOF, XRDIN
 
      READ (LUN) N,XAV,XBV
      IF (XEOF (LUN))        GO TO 61
      IF (XRDIN(LUN))        GO TO 64
      N = MIN0 (N,NXB)
      NXB = MAX0 (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -7
      RETURN
      END
