*
* $Id: vsub.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vsub.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VSUB (A,B,X,N)
C
C CERN PROGLIB# F121    VSUB            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 01/03/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*),B(*),X(*)
      PARAMETER (LVMIN =   30)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 90 I= 1,N
   90       X(I)= A(I) - B(I)

      ELSE

C*VDIR: PREFER SCALAR

         DO 95 I= 1,N
   95       X(I)= A(I) - B(I)

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VSUB
#undef CERNLIB_TCGEN_VSUB
#endif
