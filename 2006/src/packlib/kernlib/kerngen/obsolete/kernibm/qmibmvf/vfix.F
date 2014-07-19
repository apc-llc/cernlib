*
* $Id: vfix.F,v 1.1.1.1 1996/02/15 17:53:06 mclareni Exp $
*
* $Log: vfix.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:06  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VFIX (A,IA,N)
C
C CERN PROGLIB# F121    VFIX            .VERSION KERNIBM  2.27  890622
C ORIG. 03/05/72
C 01/03/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*), IA(*)
      PARAMETER (LVMIN    =   12 )

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 90 J=1,N
   90       IA(J)= A(J)

      ELSE

C*VDIR: PREFER SCALAR

         DO 95 J=1,N
   95       IA(J)= A(J)

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VFIX
#undef CERNLIB_TCGEN_VFIX
#endif
