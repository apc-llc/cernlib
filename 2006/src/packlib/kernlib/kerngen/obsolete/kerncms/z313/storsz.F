*
* $Id: storsz.F,v 1.1.1.1 1996/02/15 17:52:00 mclareni Exp $
*
* $Log: storsz.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:00  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      CHARACTER*8 FUNCTION STORSZ()
C
C CERN PROGLIB#    Z313 STORSZ          .VERSION KERNCMS  1.06  910721
C ORIG. 21/07/91, Miguel Marquina
C Extraction of Storage size
C
      CHARACTER*15 STRING
      INTEGER SYSTEMF
C
      IRC=SYSTEMF('QUERY STORAGE (LIFO')
      CALL VMRTRM(STRING,LENS)
      STORSZ=STRING(11:15)
      RETURN
      END
