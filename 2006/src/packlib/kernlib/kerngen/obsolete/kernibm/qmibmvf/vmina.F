*
* $Id: vmina.F,v 1.1.1.1 1996/02/15 17:53:07 mclareni Exp $
*
* $Log: vmina.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:07  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VMINA (A,N)
C
C CERN PROGLIB# F121    VMINA           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 24/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*)
      PARAMETER (LVMIN  =    25)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         AMIN= ABS (A(1))
         DO 90 I= 2,N
   90       AMIN = AMIN1 (ABS (A(I)),AMIN)
         VMINA= AMIN

      ELSE IF (N.GT.0) THEN
C*VDIR: PREFER SCALAR

         AMIN= ABS (A(1))
         DO 95 I= 2,N
   95       AMIN = AMIN1 (ABS (A(I)),AMIN)
         VMINA= AMIN

      ELSE

         VMINA = 0.

      END IF
      END
#ifdef CERNLIB_TCGEN_VMINA
#undef CERNLIB_TCGEN_VMINA
#endif
