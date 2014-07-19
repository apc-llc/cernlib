*
* $Id: iscan.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: iscan.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION ISCAN(STR,SET)
C
C CERN PROGLIB# M433    ISCAN           .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Scan string STR and return the leftmost position where any of
C-    the characters in SET matches a character in STR
C
      CHARACTER*(*) STR,SET
C
      DO 10 I=1,LEN(STR)
          IF (INDEX(SET,STR(I:I)).NE.0) THEN
              ISCAN  = I
              GO TO 999
          ENDIF
   10 CONTINUE
      ISCAN  = 0
C
  999 END
