*
* $Id: epsilo.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: epsilo.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE EPSILO(A,M,N,MD,EPS)
C  COMPUTATION OF AN EPSILON-VALUE
C  =====================================================================
      DOUBLE PRECISION  A(MD,1)
      EPS = 0.
      DO 2  I = 1, M
      DO 1  K = 1, N
    1 EPS = EPS + ABS(SNGL(A(I,K)))
    2 CONTINUE
      EPS = 2.*EPS/(M*N)
      ELOG = LOG10(EPS)
#if defined(CERNLIB_CDC)
      IEXP=INT(ELOG)-25
#endif
#if !defined(CERNLIB_CDC)
      IEXP=INT(ELOG)-14
#endif
      IF(ELOG.LT.0.) IEXP=IEXP-1
      EPS = 10.**IEXP
      RETURN
      END
