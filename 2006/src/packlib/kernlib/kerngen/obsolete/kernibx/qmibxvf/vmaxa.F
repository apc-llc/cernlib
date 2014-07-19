*
* $Id: vmaxa.F,v 1.1.1.1 1996/02/15 17:54:35 mclareni Exp $
*
* $Log: vmaxa.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:35  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VMAXA (A,N)
C
C CERN PROGLIB# F121    VMAXA           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 27/02/89  M.Roethlisberger/IBM         Optimization/Vectorization
C
      DIMENSION A(*)
      PARAMETER (LVMIN  =  25)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         AMAX = ABS (A(1))
         DO 90 I= 2,N
   90       AMAX = AMAX1 (ABS (A(I)),AMAX)
         VMAXA = AMAX

      ELSE IF (N.GT.0) THEN
C*VDIR: PREFER SCALAR

         AMAX = ABS (A(1))
         DO 95 I= 2,N
   95       AMAX = AMAX1 (ABS (A(I)),AMAX)
         VMAXA = AMAX

      ELSE

         VMAXA = 0.

      END IF
      END
#ifdef CERNLIB_TCGEN_VMAXA
#undef CERNLIB_TCGEN_VMAXA
#endif
