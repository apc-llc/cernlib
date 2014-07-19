*
* $Id: vbias.F,v 1.1.1.1 1996/02/15 17:54:33 mclareni Exp $
*
* $Log: vbias.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
@PROCESS DIRECTIVE ('*VDIR:') OPT(3) VECTOR
      SUBROUTINE VBIAS (B,ALPHA,A,N)
C
C CERN PROGLIB# F121    VBIAS           .VERSION KERNIBM  2.27  890622
C ORIG. 01/07/71
C 03/04/89  M.Roethlisberger/IBM        Optimize/Vectorize
C
      DIMENSION A(*),B(*)
      PARAMETER (LVMIN    =     20)
C
      IF (N.GE.LVMIN)  THEN
C*VDIR: PREFER VECTOR

       DO 90 I= 1,N
   90   A(I)= B(I) + ALPHA

      ELSE

C*VDIR: PREFER SCALAR

       DO 95 I= 1,N
   95   A(I)= B(I) + ALPHA

      ENDIF
      END
#ifdef CERNLIB_TCGEN_VBIAS
#undef CERNLIB_TCGEN_VBIAS
#endif
