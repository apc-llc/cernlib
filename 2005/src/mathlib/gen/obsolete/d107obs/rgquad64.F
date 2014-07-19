*
* $Id: rgquad64.F,v 1.1.1.1 1996/04/01 15:03:06 mclareni Exp $
*
* $Log: rgquad64.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:06  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_DOUBLE)
      FUNCTION DGQUAD(F,A,B,N)
C
#include "imp64.inc"
C
#endif
#if !defined(CERNLIB_DOUBLE)
      FUNCTION RGQUAD(F,A,B,N)
#endif
      DIMENSION X(1),W(1)
      EXTERNAL F

      CALL D107S1(1,F,A,B,N,X,W)
#if defined(CERNLIB_DOUBLE)
      DGQUAD=X(1)
#endif
#if !defined(CERNLIB_DOUBLE)
      RGQUAD=X(1)
#endif
      RETURN
      END
