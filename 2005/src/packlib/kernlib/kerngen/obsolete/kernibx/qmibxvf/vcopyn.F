*
* $Id: vcopyn.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vcopyn.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE  VCOPYN (A,X,N)
C
C CERN PROGLIB# F121    VCOPYN          .VERSION KERNIBM  2.27  890622
C ORIG. 01/04/73
C 20/03/89  M.Roethlisberger?IBM        Optimization/Vectorization
C
      DIMENSION A(*), X(*)
      PARAMETER (LVMIN   =     25)
C
      IF (N.GE.LVMIN)  THEN
C*VDIR: PREFER VECTOR
        DO 14  I=1,N
   14    X(I)= -A(I)
      ELSE

C*VDIR: PREFER SCALAR
        DO 19  I=1,N
   19    X(I)= -A(I)
      ENDIF
      END
#ifdef CERNLIB_TCGEN_VCOPYN
#undef CERNLIB_TCGEN_VCOPYN
#endif
