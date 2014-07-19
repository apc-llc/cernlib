*
* $Id: vdot.F,v 1.1.1.1 1996/02/15 17:54:34 mclareni Exp $
*
* $Log: vdot.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VDOT (X,Y,N)
C
C CERN PROGLIB# F121    VDOT            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 28/02/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION X(*),Y(*)
      REAL*8    XS
      PARAMETER (LVMIN =     28)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XV= 0.
         DO 90 I= 1,N
   90       XV= XV + X(I)*Y(I)
         VDOT=XV

      ELSE

C*VDIR: PREFER SCALAR

         XS= 0.
         DO 95 I= 1,N
   95       XS= XS + DBLE(X(I))*DBLE(Y(I))
         VDOT=XS

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VDOT
#undef CERNLIB_TCGEN_VDOT
#endif
