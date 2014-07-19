*
* $Id: pfnbb.F,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: pfnbb.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE PFNBB(IPAR,LPAR)
C
C   THIS IS ROUTINE BLANK FROM CNESLIB WITH NAME CHANGE FOR SAFETY
C   REMPLACE ZEROS BINAIRES PAR BLANCS
C
      LPAR=IPAR
      DO 100 I=1,10
#if !defined(CERNLIB_F4)
      IF ((LPAR.AND.O"77").EQ.0) LPAR=LPAR.OR.R" "
#endif
#if defined(CERNLIB_F4)
      IF ((LPAR.AND.77B).EQ.0) LPAR=LPAR.OR.1R
#endif
  100 LPAR=SHIFT(LPAR,6)
      RETURN
      END
