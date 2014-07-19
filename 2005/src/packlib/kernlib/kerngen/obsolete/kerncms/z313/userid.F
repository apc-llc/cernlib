*
* $Id: userid.F,v 1.1.1.1 1996/02/15 17:51:59 mclareni Exp $
*
* $Log: userid.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:59  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      CHARACTER*8 FUNCTION USERID()
C
C CERN PROGLIB#    Z313 USERID          .VERSION KERNCMS  1.06  910716
C ORIG. 16/07/91, Miguel Marquina
C Extraction of User Identifier
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
      USERID=STRING( 1: 8)
      RETURN
      END
