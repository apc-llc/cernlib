*
* $Id: bcdwd.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: bcdwd.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION BCDWD (NVECT,NCH)
C
C
      COMMON /SLATE/A,B(10),XQX(29)
      DIMENSION NVECT(10)
C
C
      N= NCH
      DO 7 I=1,N
    7 B(I)= AFROMI(NVECT(I))
      CALL UBUNCH (B,BCDWD,N)
      RETURN
      END
