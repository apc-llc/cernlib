*
* $Id: zabend.F,v 1.10 2001/10/08 14:03:18 mclareni Exp $
*
* $Log: zabend.F,v $
* Revision 1.10  2001/10/08 14:03:18  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ZABEND
C----------------------------------------------------------------
C-
C-      do not allow for graceful exit from a Zebra fatal error
C-
C------------------------------------------------------------------
      PRINT *,' ZABEND CALLED IS IN ISAZEB.OLB'
      CALL ABORT
      END
