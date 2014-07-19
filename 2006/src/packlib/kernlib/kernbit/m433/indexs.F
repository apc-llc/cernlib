*
* $Id: indexs.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: indexs.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION INDEXS(STRING)
C
C CERN PROGLIB# M433    INDEXS          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the first special (i.e. non numeric nor alpha-numeric)
C-                     character in a string
C
      CHARACTER*(*) STRING
C
      CHARACTER*(*) CSPEC
      PARAMETER (CSPEC=
     X '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
C
      DO 10 I=1,LEN(STRING)
         IF (INDEX(CSPEC,STRING(I:I)).EQ.0) THEN
              INDEXS = I
              GO TO 999
          ENDIF
10    CONTINUE
      INDEXS = 0
  999 END
