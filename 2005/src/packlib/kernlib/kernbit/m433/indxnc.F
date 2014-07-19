*
* $Id: indxnc.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indxnc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDXNC(STRING)
C
C CERN PROGLIB# M433    INDXNC          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the first non numeric character in a string
C
      CHARACTER*(*) STRING
C
      CHARACTER*(*) CNUM
      PARAMETER (CNUM='0123456789')
C
      DO 10 I=1,LEN(STRING)
         IF (INDEX(CNUM,STRING(I:I)).EQ.0) THEN
              INDXNC = I
              GO TO 999
          ENDIF
10    CONTINUE
      INDXNC = 0
  999 END
