*
* $Id: vmin.F,v 1.1.1.1 1996/02/15 17:53:08 mclareni Exp $
*
* $Log: vmin.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:08  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VMIN (A,N)
C
C CERN PROGLIB# F121    VMIN            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 24/02/89  M.Roethlisbeger/IBM         Optimization/Vectorization
C
      DIMENSION A(*)
      PARAMETER (LVMIN  =     30)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         AMIN= A(1)
         DO 90 I= 2,N
   90       AMIN = AMIN1 (A(I),AMIN)
         VMIN= AMIN

      ELSE IF (N.GT.0) THEN
C*VDIR: PREFER SCALAR

         AMIN= A(1)
         DO 95 I= 2,N
   95       AMIN = AMIN1 (A(I),AMIN)
         VMIN= AMIN

      ELSE

         VMIN = 0.

      END IF
      END
#ifdef CERNLIB_TCGEN_VMIN
#undef CERNLIB_TCGEN_VMIN
#endif
