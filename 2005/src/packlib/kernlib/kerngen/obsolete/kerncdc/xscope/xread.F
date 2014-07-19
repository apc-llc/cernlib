*
* $Id: xread.F,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: xread.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XREAD (ITAP,IPAR,ISTOR,JA,JE)
 
C CERN PROGLIB# Z200    XREAD           .VERSION KERNCDC  1.05  780503
 
      DIMENSION ISTOR(256)
 
 
      NWORDS = JE+1 - JA
 
      IF    (IPAR)           99, 21, 11
 
   11 CALL XREADB (ITAP,ISTOR(JA),NWORDS)
      RETURN
 
   21 CALL XREADC (ITAP,ISTOR(JA),NWORDS)
   99 RETURN
      END
