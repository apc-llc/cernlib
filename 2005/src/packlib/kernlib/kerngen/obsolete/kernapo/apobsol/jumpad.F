*
* $Id: jumpad.F,v 1.1.1.1 1996/02/15 17:51:11 mclareni Exp $
*
* $Log: jumpad.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:11  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JUMPAD (ITARGT)
C
C CERN PROGLIB# Z042    JUMPAD          .VERSION KERNAPO  1.13  880425
C ORIG. 21/04/88 JZ
C
C-    Get target address for use with JUMPXn

      JUMPAD = IADDR (ITARGT)
      END
