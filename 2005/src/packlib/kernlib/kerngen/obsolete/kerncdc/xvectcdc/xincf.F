*
* $Id: xincf.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xincf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINCF (LUN,XBV,NXB)
 
C CERN PROGLIB# Z203    XINCF           .VERSION KERNCDC  1.13  820111
 
      DIMENSION    XBV(NXB)
      LOGICAL      XEOF, XRDIN
 
      READ (LUN,8000) XBV
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
 
 8000 FORMAT (14A10)
      END
