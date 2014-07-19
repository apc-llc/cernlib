*
* $Id: jumpad.F,v 1.1.1.1 1996/02/15 17:52:27 mclareni Exp $
*
* $Log: jumpad.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JUMPAD (ITARGT)
C
C CERN PROGLIB# Z042    JUMPAD          .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C
C-    GET TARGET ADDRESS FOR USE WITH JUMPXN

      JUMPAD = %LOC (ITARGT)
      END
