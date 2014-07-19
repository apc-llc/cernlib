*
* $Id: verify.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: verify.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      INTEGER FUNCTION VERIFY(STR,SET)
C
C CERN PROGLIB# M433    VERIFY          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Scan string STR and return the leftmost position of a character
C-    in STR which is not in the set SET
C
      CHARACTER*(*) STR,SET
C
      DO 10 I=1,LEN(STR)
          IF (INDEX(SET,STR(I:I)).EQ.0) THEN
              VERIFY = I
              GO TO 999
          ENDIF
   10 CONTINUE
      VERIFY = 0
C
  999 END
