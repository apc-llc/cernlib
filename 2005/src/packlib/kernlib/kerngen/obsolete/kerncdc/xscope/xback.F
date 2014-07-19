*
* $Id: xback.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: xback.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XBACK (LUN)
 
C CERN PROGLIB# Z200    XBACK           .VERSION KERNCDC  0.1   750926
 
      BACKSPACE LUN
      RETURN
      END
