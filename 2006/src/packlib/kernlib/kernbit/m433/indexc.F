*
* $Id: indexc.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indexc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDEXC(STR,SSTR)
C
C CERN PROGLIB# M433    INDEXC          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the leftmost position where substring SSTR does not match
C-    string STR scanning forward
C
      CHARACTER*(*) STR,SSTR
C
      LENS   = LEN(STR)
      LENSS  = LEN(SSTR)
C
      DO 10 I=1,LENS-LENSS+1
          IF (STR(I:I+LENSS-1).NE.SSTR) THEN
              INDEXC = I
                                         GO TO 999
          ENDIF
   10 CONTINUE
      INDEXC = 0
C
  999 END
