*
* $Id: jumpad.F,v 1.1.1.1 1996/02/15 17:53:23 mclareni Exp $
*
* $Log: jumpad.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JUMPAD (ITARGT)
C
C CERN PROGLIB# Z042    JUMPAD          .VERSION KERNIBM  2.23  880425
C ORIG. 21/04/88 JZ
C
C-    Get target address for use with JUMPXn

      JUMPAD = ITARGT
      END
