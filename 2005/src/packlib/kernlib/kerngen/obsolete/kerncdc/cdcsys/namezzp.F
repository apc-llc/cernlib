*
* $Id: namezzp.F,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: namezzp.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE NAMEZZP (A)
      DIMENSION A(12)
 
C CERN PROGLIB#         NAMEZZP         .VERSION KERNCDC  1.08  790126
 
      PRINT 9000, A
      RETURN
 
 9000 FORMAT (12A10)
      END
