*
* $Id: indxbc.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indxbc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDXBC(STR,SSTR)
C
C CERN PROGLIB# M433    INDXBC          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the leftmost position where substring SSTR does not match
C-    string STR scanning from back to front starting at the end of STR
C
      CHARACTER*(*) STR,SSTR
C
      LENS   = LEN(STR)
      LENSS  = LEN(SSTR)
C
      DO 10 I=LENS-LENSS+1,1,-1
          IF (STR(I:I+LENSS-1).NE.SSTR) THEN
              INDXBC = I
                                         GO TO 999
          ENDIF
   10 CONTINUE
      INDXBC = 0
C
  999 END
