*
* $Id: vadd.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vadd.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VADD (B,C,A,N)
C
C CERN PROGLIB# F121    VADD            .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 31/03/89  M.Roethlisberger/IBM        Optimize/Vectorize
C
      DIMENSION A(*),B(*),C(*)
      PARAMETER (LVMIN =    15)
C
      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

       DO 90 I= 1,N
   90   A(I)= B(I) + C(I)

       ELSE

C*VDIR: PREFER SCALAR

       DO 95 I= 1,N
   95   A(I)= B(I) + C(I)

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VADD
#undef CERNLIB_TCGEN_VADD
#endif
