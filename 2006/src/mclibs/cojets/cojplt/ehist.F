*
* $Id: ehist.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: ehist.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE EHIST
C     ****************
C
C          PRINT HBOOK HISTOGRAMS
C
#include "cojets/itapes.inc"
      COMMON/MXHIST/MXHIST,NHSHFT
      LOGICAL HEXIST
C
      DO 100 I=1,MXHIST
      IF(HEXIST(I)) CALL HDELET(I)
100   CONTINUE
C
      CALL HISTDO
C
      RETURN
      END
