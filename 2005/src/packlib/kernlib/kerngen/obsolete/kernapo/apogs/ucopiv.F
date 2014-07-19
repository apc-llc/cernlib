*
* $Id: ucopiv.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: ucopiv.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UCOPIV (A,B,N)
C
C CERN PROGLIB# V301    UCOPIV          .VERSION KERNAPO  1.17  890531
C ORIG. 01/07/71
C MOD   31/05/89 FCA
C
      DIMENSION A(*), B(*)
C
      IF (N.GT.0) CALL VEC_$COPY_I (A,1,B(N),-1,N)
      END
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
