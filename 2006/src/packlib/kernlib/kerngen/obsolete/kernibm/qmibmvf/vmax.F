*
* $Id: vmax.F,v 1.1.1.1 1996/02/15 17:53:07 mclareni Exp $
*
* $Log: vmax.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:07  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VMAX (A,N)
C
C CERN PROGLIB# F121    VMAX            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 27/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*)
      PARAMETER (LVMIN  =  30)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         AMAX = A(1)
         DO 90 I= 2,N
   90       AMAX = AMAX1 (A(I),AMAX)
         VMAX = AMAX

      ELSE IF (N.GT.0) THEN
C*VDIR: PREFER SCALAR

         AMAX = A(1)
         DO 95 I= 2,N
   95       AMAX = AMAX1 (A(I),AMAX)
         VMAX = AMAX

      ELSE

         VMAX = 0.

      END IF
      END
#ifdef CERNLIB_TCGEN_VMAX
#undef CERNLIB_TCGEN_VMAX
#endif
