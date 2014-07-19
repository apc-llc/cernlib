*
* $Id: vdist.F,v 1.1.1.1 1996/02/15 17:53:07 mclareni Exp $
*
* $Log: vdist.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:07  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VDIST (X,Y,N)
C
C CERN PROGLIB# F121    VDIST           .VERSION KERNIBM  2.27  890622
C ORIG. 26/09/76
C 28/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION X(*), Y(*)
      REAL*8    XX8
      PARAMETER (LVMIN  =     27)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XX = 0.
         DO 90 J=1,N
   90       XX = XX + (X(J)-Y(J)) **2
         XX = SQRT (XX)
         VDIST = XX
      ELSE

C*VDIR: PREFER SCALAR

         XX8 = 0.
         DO 95 J=1,N
   95       XX8 = XX8 + (DBLE(X(J)-Y(J))) **2
         XX = SQRT (SNGL(XX8))
         VDIST = XX
      ENDIF
      END
#ifdef CERNLIB_TCGEN_VDIST
#undef CERNLIB_TCGEN_VDIST
#endif
