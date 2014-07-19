*
* $Id: indexb.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indexb.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDEXB(STR,SSTR)
C
C CERN PROGLIB# M433    INDEXB          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the first occurrence of the substring SSTR in the string STR
C-    starting at the back of STR and scanning backward
C
      CHARACTER*(*) STR,SSTR
C
      LENS   = LEN(STR)
      LENSS  = LEN(SSTR)
C
      DO 10 I=LENS-LENSS+1,1,-1
          IF (STR(I:I+LENSS-1).EQ.SSTR) THEN
              INDEXB = I
                                         GO TO 999
          ENDIF
   10 CONTINUE
      INDEXB = 0
C
  999 END
