*
* $Id: fmupdc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmupdc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUPDC
#include "fatmen/fatsys.inc"
#include "fatmen/fatinit.inc"
      CALL KUGETI(MAX)
      CALL KUGETI(NGROUP)
      CALL KUGETI(IFLAG)
      CALL FMUPDT(MAX,NGROUP,IFLAG,IRC)
      IF (IRC .NE. 0) PRINT *,'Return code ',IRC,' from FMUPDT'
      END
