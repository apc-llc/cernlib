*
* $Id: hqixyz.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hqixyz.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.00/00 20/11/92  12.56.34  by  John Allison
*-- Author :
      SUBROUTINE HQIXYZ (VALUE, RMIN, RMAX)
*     IMPLICIT NONE
      REAL VALUE, RMIN (*), RMAX (*)
* Initialises isosurface value VALHQF IN /PAWNPU/ for FUNCTION/DRAW with HQFXYZ.
* Also fills ranges RMIN, RMAX.

#include "hbook/hcflag.inc"
#include "paw/pawnpu.inc"

      INTEGER NC(3)

      VALHQF = VALUE
      IDIM = 3
      CALL HQGIV (IDIM, NC, RMIN, RMAX)
      IF (IDIM .NE. 3) CALL HBUG ('IDIM should be 3', 'HQIXYZ', ID)

      END
