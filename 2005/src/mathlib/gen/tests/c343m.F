*
* $Id: c343m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c343m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C343M
      LOGICAL LTEST, LTEST1,LTEST2,LTEST3
      COMMON /C343LT1/LTEST1
      COMMON /C343LT2/LTEST2
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      COMMON /C343LT3/LTEST3
#endif
#include "iorc.inc"
      CALL HEADER('C343',0)
      LTEST=.TRUE.
      LTEST1=.TRUE.
      LTEST2=.TRUE.
      LTEST3=.TRUE.
      CALL C343S
        LTEST=LTEST .AND. LTEST1
      CALL C343D
        LTEST=LTEST .AND. LTEST2
#if (defined(CERNLIB_QUAD))&&(defined(CERNLIB_DOUBLE))
      CALL C343Q
        LTEST=LTEST .AND. LTEST3
#endif
      IRC=ITEST('C343',LTEST)
      CALL PAGEND('C343')
      RETURN
      END
