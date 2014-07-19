*
* $Id: ffuset.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffuset.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFUSET(FUSUB)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine stores the address of the user
C              routine which should be called when a key
C              is preceded by an *. This routine can be
C              called at any time to redefine the address
C              of the user routine, but AFTER FFINIT. If
C              this routine is not called, the default
C              routine FFUSER will be used
C
C      DUMMY ARGUMENTS:
C
C              FUSUB - routine to be called (to be declared
C                      EXTERNAL in the caling program)
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
#include "ffread/ffcomm.inc"
#include "ffread/ffmach.inc"
C
      KUJUMP = JUMPAD(FUSUB)
C
      END
