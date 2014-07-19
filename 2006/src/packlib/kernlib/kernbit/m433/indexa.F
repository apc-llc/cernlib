*
* $Id: indexa.F,v 1.1.1.1 1996/02/15 17:47:48 mclareni Exp $
*
* $Log: indexa.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:48  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDEXA(STRING)
C
C CERN PROGLIB# M433    INDEXA          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the first alpha-numeric character in a string
C
      CHARACTER*(*) STRING
C
      CHARACTER*(*) CALFA
      PARAMETER
     X (CALFA='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
C
      DO 10 I=1,LEN(STRING)
         IF (INDEX(CALFA,STRING(I:I)).NE.0) THEN
              INDEXA = I
              GO TO 999
          ENDIF
10    CONTINUE
      INDEXA = 0
  999 END
