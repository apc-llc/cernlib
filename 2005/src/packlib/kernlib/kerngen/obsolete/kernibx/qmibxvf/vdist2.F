*
* $Id: vdist2.F,v 1.1.1.1 1996/02/15 17:54:34 mclareni Exp $
*
* $Log: vdist2.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VDIST2 (X,Y,N)
C
C CERN PROGLIB# F121    VDIST2          .VERSION KERNIBM  2.27  890622
C ORIG. 26/09/76
C 28/02/89  M.Roethlisberger            Optimization/Vectorization
C
      DIMENSION X(*), Y(*)
      REAL*8    XX8
      PARAMETER (LVMIN  =     27)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XX = 0.
         DO 90 J=1,N
   90       XX = XX + (X(J)-Y(J)) **2
         VDIST2 = XX
       ELSE

C*VDIR: PREFER SCALAR

         XX8 = 0.
         DO 95 J=1,N
   95       XX8 = XX8 + (DBLE(X(J)-Y(J))) **2
         VDIST2 = XX8
      ENDIF
      RETURN
      END
#ifdef CERNLIB_TCGEN_VDIST2
#undef CERNLIB_TCGEN_VDIST2
#endif
