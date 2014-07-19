*
* $Id: lenocc.F,v 1.1.1.1 1996/02/26 17:16:54 mclareni Exp $
*
* $Log: lenocc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:54  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if !defined(CERNLIB_PAW)
*CMZ :  1.18/14 18/10/94  11.58.38  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   18/10/94
      FUNCTION LENOCC (CHV)
C
C CERN PROGLIB# M507    LENOCC          .VERSION KERNFOR  4.21  890323
C ORIG. March 85, A.Petrilli, re-write 21/02/89, JZ
C
C-    Find last non-blank character in CHV

      CHARACTER    CHV*(*)

      N = LEN(CHV)

      DO 17  JJ= N,1,-1
      IF (CHV(JJ:JJ).NE.' ') GO TO 99
   17 CONTINUE
      JJ = 0

   99 LENOCC = JJ
      RETURN
      END
#endif
