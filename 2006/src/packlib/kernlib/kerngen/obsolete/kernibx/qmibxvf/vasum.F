*
* $Id: vasum.F,v 1.1.1.1 1996/02/15 17:54:35 mclareni Exp $
*
* $Log: vasum.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:35  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VASUM (A,N)
C
C CERN PROGLIB# F121    VASUM           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 28/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*)
      REAL*8    XS
      PARAMETER (LVMIN  =    40)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XV= 0.
         DO 90 I= 1,N
   90       XV= XV + ABS (A(I))
         VASUM=XV

      ELSE
C
C*VDIR: PREFER SCALAR

         XS= 0.
         DO 95 I= 1,N
   95       XS= XS + ABS (A(I))
         VASUM=XS

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VASUM
#undef CERNLIB_TCGEN_VASUM
#endif
