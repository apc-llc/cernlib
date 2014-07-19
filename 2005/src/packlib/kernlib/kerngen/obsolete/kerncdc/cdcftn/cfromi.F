*
* $Id: cfromi.F,v 1.1.1.1 1996/02/15 17:51:19 mclareni Exp $
*
* $Log: cfromi.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:19  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_F77)
      CHARACTER*(*) FUNCTION CFROMI(I)
C
C CERN PROGLIB# M431    CFROMI          .VERSION KERNCDC  2.16  860514
C ORIG. 10/02/86 M.METCALF
C
C   TO CONVERT 4 PACKED ASCII CODES TO CHARACTERS
C   CDC ONLY
C
      PARAMETER (M8 = COMPL(MASK(52)))
      CFROMI = ' '
      J = I
      LENCH = MIN(LEN(CFROMI), 4)
      JJ = LENCH
      DO 1 II = 4, 1, -1
         IF (II.LE.LENCH) THEN
            CALL ITOCH(AND(J, M8), CFROMI(JJ:JJ), *999)
            JJ = JJ-1
         ENDIF
         J = SHIFT(J, -8)
    1 CONTINUE
  999 END
#ifdef CERNLIB_TCGEN_CFROMI
#undef CERNLIB_TCGEN_CFROMI
#endif
#endif
