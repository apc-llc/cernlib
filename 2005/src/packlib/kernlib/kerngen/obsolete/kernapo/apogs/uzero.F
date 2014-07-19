*
* $Id: uzero.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: uzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UZERO (IV,JL,JR)
C
C CERN PROGLIB# V300    UZERO           .VERSION KERNAPO  1.17  890701
C ORIG. 01/01/65 JZ
C MOD   31/05/89 FCA
C
      DIMENSION  IV(*)
C
      NCOUNT=JR-JL+1
      IF(NCOUNT.GT.0) CALL VEC_$INIT (IV(JL),NCOUNT,0)
      END
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
