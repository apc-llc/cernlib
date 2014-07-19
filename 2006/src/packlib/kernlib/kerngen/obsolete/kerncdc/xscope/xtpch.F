*
* $Id: xtpch.F,v 1.1.1.1 1996/02/15 17:51:18 mclareni Exp $
*
* $Log: xtpch.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:18  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XTPCH (LUN)
 
C CERN PROGLIB# Z200    XTPCH           .VERSION KERNCDC  0.1   750926
 
      CALL CLVLZL (LUN,IFLAG)
      RETURN
      END
