*
* $Id: vexcum.F,v 1.1.1.1 1996/02/15 17:54:34 mclareni Exp $
*
* $Log: vexcum.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VEXCUM (A,EX,N)

C
C CERN PROGLIB# F121    VEXCUM          .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 20/03/89  M.ROETHLISBERGER/IBM        OPTIMIZATION/VECTORIZATION
C
      DIMENSION A(*), EX(3)
      PARAMETER (LVMIN  =   45 )

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 190 J=1,N
            EX(1)= AMIN1(EX(1),A(J))
            EX(2)= AMAX1(EX(2),A(J))
  190       EX(3)= EX(3) + A(J)

      ELSE

C*VDIR: PREFER SCALAR

         DO 195 J=1,N
            EX(1)= AMIN1(EX(1),A(J))
            EX(2)= AMAX1(EX(2),A(J))
  195       EX(3)= EX(3) + A(J)

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VEXCUM
#undef CERNLIB_TCGEN_VEXCUM
#endif
