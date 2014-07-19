*
* $Id: c302q.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c302q.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_QUAD)
      SUBROUTINE C302Q
#include "gen/imp128.inc"
      LOGICAL LTEST2

      DIMENSION C(0:20)
      CHARACTER*6 TFUNC

      PARAMETER (Z1 = 1)
      COMMON /C302LT2/LTEST2
#include "iorc.inc"

#if defined(CERNLIB_DOUBLE)
      DATA TFUNC/'QGAMMA'/
#endif
#if !defined(CERNLIB_DOUBLE)
      DATA TFUNC/'DGAMMA'/
#endif
      LTEST2=.TRUE.
#if defined(CERNLIB_DOUBLE)
      ETOL=1Q-31
      PI = 3.14159 26535 89793 23846 26433 83279 50Q0
         ERMAX= 0.0Q0
         WRITE(LOUT,'(/10X,''Test of C302 '',A)') TFUNC
#endif
#if !defined(CERNLIB_DOUBLE)
      ETOL=1D-28
      PI = 3.14159 26535 89793 23846 26433 83279 50D0
         ERMAX= 0.0D0
         WRITE(LOUT,'(/10X,''Test of C302 '',A)') TFUNC
#endif
         WRITE(LOUT,'(/9X,''X  '',7X,''Exact'',28X,''Calculated'',
     + 18X,''Rel. Error'')')
      C(0)=1
      DO 2 N = 1,20
    2 C(N)=(2*N-1)*C(N-1)/2
      DO 1 N = 0,20
      X=N+Z1/2
      T=C(ABS(N))*SQRT(PI)
#if defined(CERNLIB_DOUBLE)
      R=QGAMMA(X)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=DGAMMA(X)
#endif
         IF(R .NE. 0) ER=ABS((R-T)/R)
      ERMAX= MAX( ERMAX,ER )
      WRITE(LOUT,'(1X,F10.1,2E40.32,5X,1P,E10.1)') X,T,R,ER
    1 CONTINUE
      WRITE(LOUT,'(/''Largest relative error was'',E10.1)') ERMAX
      LTEST2=LTEST2.AND.(ERMAX.LE.ETOL)
      WRITE(LOUT,'(/''Testing error messages:'')')
#if defined(CERNLIB_DOUBLE)
      R=QGAMMA(-Z1/2)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=DGAMMA(-Z1/2)
#endif
      RETURN
      END
#endif
