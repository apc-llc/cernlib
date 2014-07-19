*
* $Id: vsum.F,v 1.1.1.1 1996/02/15 17:53:07 mclareni Exp $
*
* $Log: vsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:07  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      FUNCTION VSUM (A,N)
C
C CERN PROGLIB# F121    VSUM            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 20/02/89  M.Roethlisberger/IBM        Optimization/vectorization
C
      DIMENSION A(*)
      REAL*8    XS
      PARAMETER (LVMIN = 45)
C
      IF (N.GE.LVMIN)  THEN
C*VDIR: PREFER VECTOR

         XV = 0.
         DO 90 I= 1,N
   90       XV = XV + A(I)

         VSUM=XV
      ELSE

C*VDIR: PREFER SCALAR

         XS=0.
         DO 95 I= 1,N
   95       XS= XS + A(I)

         VSUM=XS
      ENDIF
      END
#ifdef CERNLIB_TCGEN_VSUM
#undef CERNLIB_TCGEN_VSUM
#endif
