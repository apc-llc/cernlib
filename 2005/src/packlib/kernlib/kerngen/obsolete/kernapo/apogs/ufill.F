*
* $Id: ufill.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: ufill.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UFILL (IV,JL,JR,ISTUFF)
C
C CERN PROGLIB# V300    UFILL           .VERSION KERNAPO  1.17  890531
C ORIG. 01/07/71 JZ
C MOD   31/05/89 FCA
C
      DIMENSION  IV(*)
C
      NCOUNT=JR-JL+1
      IF(NCOUNT.GT.0) CALL VEC_$INIT (IV(JL),NCOUNT,ISTUFF)
      END
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
