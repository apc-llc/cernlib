*
* $Id: lnblnk.F,v 1.1.1.1 1996/02/15 17:50:38 mclareni Exp $
*
* $Log: lnblnk.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:38  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION LNBLNK (CHV)
C
C CERN PROGLIB# M432    LNBLNK          .VERSION KERNFOR  4.21  890323
C ORIG. 04/10/88, JZ
C
C-    Find last non-blank character in CHV

      CHARACTER    CHV*(*)

      N = LEN(CHV)

      DO 17  JJ= N,1,-1
      IF (CHV(JJ:JJ).NE.' ') GO TO 99
   17 CONTINUE
      JJ = 0

   99 LNBLNK = JJ
      RETURN
      END
