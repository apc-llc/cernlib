*
* $Id: rscsid.F,v 1.1.1.1 1996/02/15 17:51:59 mclareni Exp $
*
* $Log: rscsid.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:59  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      CHARACTER*8 FUNCTION RSCSID()
C
C CERN PROGLIB#    Z313 RSCSID          .VERSION KERNCMS  1.06  910716
C ORIG. 16/07/91, Miguel Marquina
C Extraction of RSCS Identifier
C
      COMMON /Z313ID/ IREAD, STRING
      CHARACTER*40 STRING
      INTEGER SYSTEMF
C
      IF(IREAD.NE.1) THEN
        IRC=SYSTEMF('IDENTIFY (LIFO')
        CALL VMRTRM(STRING,LENS)
        IREAD=1
      ENDIF
      RSCSID=STRING(26:33)
      RETURN
      END
