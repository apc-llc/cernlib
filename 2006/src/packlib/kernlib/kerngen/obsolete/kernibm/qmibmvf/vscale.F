*
* $Id: vscale.F,v 1.1.1.1 1996/02/15 17:53:05 mclareni Exp $
*
* $Log: vscale.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:05  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VSCALE  (B,ALPHA,A,N)
C
C CERN PROGLIB# F121    VSCALE          .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 01/03/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*),B(*)
      PARAMETER (LVMIN   =   30)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 90  I= 1,N
   90       A(I)= B(I) * ALPHA

      ELSE

C*VDIR: PREFER SCALAR

         DO 95  I= 1,N
   95       A(I)= B(I) * ALPHA

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VSCALE
#undef CERNLIB_TCGEN_VSCALE
#endif
