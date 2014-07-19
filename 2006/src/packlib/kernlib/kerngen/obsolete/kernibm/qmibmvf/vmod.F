*
* $Id: vmod.F,v 1.1.1.1 1996/02/15 17:53:07 mclareni Exp $
*
* $Log: vmod.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:07  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VMOD (A,N)
C
C CERN PROGLIB# F121    VMOD            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 24/02/89  M.Roethlisberger/IBM       Optimization/Vectorization
C
      DIMENSION A(*)
      REAL*8    XS
      PARAMETER (LVMIN =   30)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR
         XV = 0.

            DO 90 I=1,N
   90          XV = XV + A(I)*A(I)

         VMOD=SQRT(XV)
      ELSE

C*VDIR: PREFER SCALAR

         XS = 0.
            DO 95 I=1,N
   95          XS = XS + DBLE(A(I))*DBLE(A(I))

         VMOD=SQRT(SNGL(XS))
      ENDIF
      END
#ifdef CERNLIB_TCGEN_VMOD
#undef CERNLIB_TCGEN_VMOD
#endif
