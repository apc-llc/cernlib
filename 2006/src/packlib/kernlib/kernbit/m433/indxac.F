*
* $Id: indxac.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indxac.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDXAC(STRING)
C
C CERN PROGLIB# M433    INDXAC          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the first non alpha-numeric character in a string
C
      CHARACTER*(*) STRING
C
      CHARACTER*(*) CALFA
      PARAMETER
     X (CALFA='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
C
      DO 10 I=1,LEN(STRING)
         IF (INDEX(CALFA,STRING(I:I)).EQ.0) THEN
              INDXAC = I
              GO TO 999
          ENDIF
10    CONTINUE
      INDXAC = 0
  999 END
