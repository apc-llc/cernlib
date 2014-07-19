*
* $Id: fzclos.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: fzclos.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE FZCLOS(LUN,CHOPT,IRC)
*
*     Close Zebra file on unit LUN with call to FZENDx
*
      CHARACTER*6 CHSTAT
      CHARACTER*8 CHOPF
#include "cspack/czsock.inc"
#include "cspack/czopts.inc"
*
*     Is the file accessed via a server
*
      IF(JSKIN(LUN).NE.0) THEN
         CALL XZCLOS(LUN,CHOPT,IRC)
         ELSE
         CHSTAT = 'KEEP'
         IF(IOPTD.NE.0) CHSTAT = 'DELETE'
         CLOSE(LUN,STATUS=CHSTAT)
         ENDIF
 
      END
 
