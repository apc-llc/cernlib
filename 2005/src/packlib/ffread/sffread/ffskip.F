*
* $Id: ffskip.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffskip.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFSKIP (BUFFER, KURSOR)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine skips over blanks in BUFFER, starting
C              at position KURSOR. The position of the first non-blank
C              character is returned in KURSOR; if none is found, KURSOR
C              will equal LEN (BUFFER) + 1.
C
C      DUMMY ARGUMENTS:
C
C              BUFFER - passed length character string
C              KURSOR - input:  starting position
C                       output: position of first non-blank character
C
C      IMPLICIT INPUTS:
C
C              NONE
C
C      IMPLICIT OUTPUTS:
C
C              NONE
C
C      SIDE EFFECTS:
C
C              NONE
C
      CHARACTER BUFFER*(*)
C
C----------------- Beginning of executable statements -------------------------
C
C
C        Loop over all characters remaining
C
      DO 1 I = KURSOR, LEN(BUFFER)
          IF (BUFFER(I:I) .NE. ' '
#if defined(CERNLIB_TABS)
     *       .AND. ICHAR (BUFFER(I:I)) .NE. 9
#endif
     *       ) GO TO 2
1     CONTINUE
 
2     KURSOR = I
 
      END
