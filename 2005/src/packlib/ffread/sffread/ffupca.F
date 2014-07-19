*
* $Id: ffupca.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffupca.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
#if defined(CERNLIB_UPLOW)
      SUBROUTINE FFUPCA (STRING, IPOS, LENGTH)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine will convert all characters in
C              STRING(IPOS:IPOS+LENGTH-1) to upper case.
C
C      DUMMY ARGUMENTS:
C
C              STRING - passed length character string to be converted
C                       to upper case (done in place)
C              IPOS   - starting position
C              LENGTH - number of characters to convert
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
#if !defined(CERNLIB_EBCDIC)
      PARAMETER (IDIFF = -32)
#endif
#if defined(CERNLIB_EBCDIC)
      PARAMETER (IDIFF =  64)
#endif
      CHARACTER STRING*(*)
C
C----------------- Beginning of executable statements -------------------------
C
C        Loop over characters and substitute if lower case
C
      DO 1 I = IPOS, IPOS + LENGTH - 1
          J = ICHAR (STRING(I:I))
          IF (J .GE. ICHAR ('a') .AND. J .LE. ICHAR ('z'))
     *        STRING(I:I) = CHAR (J + IDIFF)
1     CONTINUE
 
      END
#endif
