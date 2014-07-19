*
* $Id: xrew.F,v 1.1.1.1 1996/02/15 17:51:18 mclareni Exp $
*
* $Log: xrew.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:18  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XREW (LUN)
 
C CERN PROGLIB# Z200    XREW            .VERSION KERNCDC  0.1   750926
 
      REWIND LUN
      RETURN
      END
