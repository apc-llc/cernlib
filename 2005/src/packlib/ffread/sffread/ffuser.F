*
* $Id: ffuser.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffuser.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFUSER (KEY)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This is a dummy user routine which does nothing.
C
C      DUMMY ARGUMENTS:
C
C              KEY - This is an array containing the current key
C                    stored in hollerith, blank padded to the
C                    defined length of a key.
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
      INTEGER KEY(1)
 
      END
