*
* $Id: vunit.F,v 1.1.1.1 1996/02/15 17:53:06 mclareni Exp $
*
* $Log: vunit.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:06  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VUNIT (A,B,N)
C
C CERN PROGLIB# F121    VUNIT           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 01/03/89  M.Roethlisberger/IBM         Optimization/Vectorization
C
      DIMENSION A(*), B(*)
      REAL*8    XX8
      PARAMETER (LVMIN  =     30)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         XX= 0.
         DO 90 I=1,N
   90       XX= XX + A(I)*A(I)

         IF (XX.LE.0.)  RETURN
         YY=1./ SQRT (XX)

C*VDIR: PREFER VECTOR

         DO 190 I = 1,N
  190       B(I)=A(I) * YY

      ELSE

C*VDIR: PREFER SCALAR ON

         XX8= 0.
         DO 95 I=1,N
   95       XX8= XX8 + DBLE(A(I))*DBLE(A(I))

         IF (XX8.LE.0.)  RETURN
         YY=1./ SQRT (SNGL(XX8))

         DO 195 I = 1,N
  195       B(I)=A(I) * YY

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VUNIT
#undef CERNLIB_TCGEN_VUNIT
#endif
