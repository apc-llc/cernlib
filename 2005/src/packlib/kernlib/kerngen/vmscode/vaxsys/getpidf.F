*
* $Id: getpidf.F,v 1.1.1.1 1996/02/15 17:50:26 mclareni Exp $
*
* $Log: getpidf.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:26  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE GETPIDF (IPID)

C CERN PROGLIB# Z265    GETPIDF         .VERSION KERNVAX  2.46  940913
C ORIG. 31/05/94, JS

C-    Simulate UNIX system call

      INCLUDE '($JPIDEF)'

      CALL LIB$GETJPI(JPI$_PID,,,IPID,,)
      RETURN
      END
