*
* $Id: vlinco.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vlinco.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VLINCO (B,ALPHA,C,BETA,A,N)
C
C CERN PROGLIB# F121    VLINCO          .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 01/03/89  M.Roethlisberger/IBM        Optimization/Vectorization
C
      DIMENSION A(*),B(*),C(*)
      PARAMETER (LVMIN   = 20)

      IF (N.GE.LVMIN) THEN
C*VDIR: PREFER VECTOR

         DO 90 I= 1,N
   90       A(I)= B(I)*ALPHA + C(I)*BETA

      ELSE

C*VDIR: PREFER SCALAR

         DO 95 I= 1,N
   95       A(I)= B(I)*ALPHA + C(I)*BETA

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VLINCO
#undef CERNLIB_TCGEN_VLINCO
#endif
