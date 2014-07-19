*
* $Id: nodeid.F,v 1.1.1.1 1996/02/15 17:51:59 mclareni Exp $
*
* $Log: nodeid.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:59  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      CHARACTER*8 FUNCTION HOSTID()
C
C CERN PROGLIB#    Z313 HOSTID          .VERSION KERNCMS  1.06  910716
C ORIG. 16/07/91, Miguel Marquina
C Extraction of Host Identifier
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
      HOSTID=STRING(13:20)
      RETURN
      END
