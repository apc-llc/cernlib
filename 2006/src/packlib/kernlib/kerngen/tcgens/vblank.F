*
* $Id: vblank.F,v 1.2 2006/09/15 09:35:23 mclareni Exp $
*
* $Log: vblank.F,v $
* Revision 1.2  2006/09/15 09:35:23  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE VBLANK (A,N)
C
C CERN PROGLIB# F121    VBLANK          .VERSION KERNFOR  4.08  840613
C ORIG. 01/07/71
C
      INTEGER      A(*), BLANK
      DATA  BLANK  / 4H     /
#if defined(CERNLIB_GFORTRAN)
      save
#endif
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 A(I)= BLANK
      RETURN
      END
