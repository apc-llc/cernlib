*
* $Id: repeat.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: repeat.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION REPEAT(STR,NTIMES)
C
C CERN PROGLIB# M433    REPEAT          .VERSION KERNFOR  4.14  860211
C ORIG. 27/03/86 M.GOOSSENS/DD
C
C-    The function value REPEAT returns NTIMES concatenated copies
C-    of the input string STR
C
      CHARACTER*(*) STR
C
      IBEGIN = 1
      LENSTR = LEN(STR)
      LENREP = LEN(REPEAT)
      DO 10 I=1,MIN(NTIMES,LENREP/LENSTR+1)
          REPEAT(IBEGIN:) = STR
          IBEGIN = IBEGIN + LENSTR
   10 CONTINUE
C
  999 END
