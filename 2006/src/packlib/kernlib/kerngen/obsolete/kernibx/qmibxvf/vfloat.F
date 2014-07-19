*
* $Id: vfloat.F,v 1.1.1.1 1996/02/15 17:54:34 mclareni Exp $
*
* $Log: vfloat.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:34  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VFLOAT (IA,A,N)
C
C CERN PROGLIB# F121    VFLOAT          .VERSION KERNIBM  2.27  890622
C ORIG. 03/05/72
C 01/03/89  M.Roethlisberger/IBM        Optimization/Vectorizatin
C
      DIMENSION A(*), IA(*)
      PARAMETER (LVMIN  =    10)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 90 J=1,N
   90       A(J)= IA(J)

      ELSE

C*VDIR: PREFER SCALAR

         DO 95 J=1,N
   95       A(J)= IA(J)

      ENDIF

      END
#ifdef CERNLIB_TCGEN_VFLOAT
#undef CERNLIB_TCGEN_VFLOAT
#endif
