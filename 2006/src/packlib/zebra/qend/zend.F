*
* $Id: zend.F,v 1.2 1999/06/18 13:31:11 couet Exp $
*
* $Log: zend.F,v $
* Revision 1.2  1999/06/18 13:31:11  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:14  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE ZEND

C-    DEFAULT FOR THE USER ROUTINE FOR PROGRAM STOP

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
*


      CALL ZPHASE (-1)
      CALL MZEND

      IF (NQERR.EQ.0)              GO TO 99
      CALL ABEND
   99 CONTINUE
      STOP
      END
