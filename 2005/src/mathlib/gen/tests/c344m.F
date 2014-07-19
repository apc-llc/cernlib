*
* $Id: c344m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c344m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_IBMVM)||!defined(CERNLIB_DOUBLE)
      SUBROUTINE C344M
C     Routine to test MATHLIB subroutines CBSJA and WBSJA (C344)
      LOGICAL LTEST, LTEST1,LTEST2
      COMMON /C344LT1/LTEST1
      COMMON /C344LT2/LTEST2
#include "iorc.inc"
      CALL HEADER('C344',0)
      LTEST=.TRUE.
      LTEST1=.TRUE.
      LTEST2=.TRUE.
      CALL C344Q
#if defined(CERNLIB_DOUBLE)
        LTEST=LTEST .AND. LTEST1
#endif
#if !defined(CERNLIB_DOUBLE)
        LTEST=LTEST .AND. LTEST2
#endif
      IRC=ITEST('C344',LTEST)
      CALL PAGEND('C344')
      RETURN
      END
#endif
