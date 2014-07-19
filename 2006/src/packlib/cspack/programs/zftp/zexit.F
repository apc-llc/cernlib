*
* $Id: zexit.F,v 1.1.1.1 1996/03/08 15:44:18 mclareni Exp $
*
* $Log: zexit.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZEXIT
*
*                Called by KUIP/EXIT to close connections and files.
*
#include "cspack/czsock.inc"
      SAVE IFIRST
      DATA IFIRST/0/
*________________________________________________________
*
*
      IF(IFIRST.EQ.0)THEN
*
*           Close remote connection
*
         IF(ISKIN.GE.0)CALL CZCLOS(ISTAT)
         IFIRST=1
      ENDIF
*
      END
 
