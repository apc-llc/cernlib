*
* $Id: zexit.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: zexit.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
      SUBROUTINE ZEXIT
*
*                Called by KUIP/EXIT to close connections and files.
*
#include "czsock.inc"
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
*        PRINT *,'Moi, je ne regrette rien'
      ENDIF
*
      END
 
