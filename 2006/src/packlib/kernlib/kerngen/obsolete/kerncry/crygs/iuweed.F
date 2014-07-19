*
* $Id: iuweed.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: iuweed.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION IUWEED (IA,N)
C
C CERN PROGLIB# N103    IUWEED          .VERSION KERNCRY  1.16  911111
C ORIG. 24/10/91 JZ
C-    Find position of first number which is infinity or not-a-number
C-    in the N word vector IA, return zero if none

      DIMENSION IA(*)

      DO  24  J=1,N
      M = SHIFTR (IA(J),48)
      IF (M.EQ.0)           GO TO 24
      IF (M.EQ.177777B)     GO TO 24

      M = M .AND. 77777B
      IF (M.GE.60000B)      GO TO 29
   24 CONTINUE
      J = 0

   29 IUWEED = J
      RETURN
      END
#ifdef CERNLIB_TCGEN_IUWEED
#undef CERNLIB_TCGEN_IUWEED
#endif
