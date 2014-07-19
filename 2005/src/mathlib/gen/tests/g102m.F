*
* $Id: g102m.F,v 1.1.1.1 1996/04/01 15:01:28 mclareni Exp $
*
* $Log: g102m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:28  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G102M
C     Routine to test MATHLIB routine PROBKL (G102)

      LOGICAL LTEST
#include "iorc.inc"
C     Set numerical tolerance for test to be successful
      DATA TOL / 1E-5/
      CALL HEADER('G102',0)
      LTEST=.TRUE.
      ERMAX=0E0
      WRITE (LOUT,'(/8X,''X'',6X,''PROBKL'',9X,''TEST'',
     +10X,''Error'')')
      DO 1 I = 1,40,5
      X=I/10.
      R=PROBKL(X)
      T=0
      DO 2 J = 1,50
      A=-1*((-1)**(J)*EXP(-2*(J*X)**2))
      T=T+A
      IF(ABS(A) .LE. 1E-10*R) GO TO 3
    2 CONTINUE
    3 T=2*T
      E=0
      IF(R .NE. 0) E=ABS((R-T)/R)
      ERMAX=MAX(ERMAX,E)
      WRITE(LOUT,'(1X,F10.2,1P,2E15.7,E10.1)') X,R,T,E
    1 CONTINUE
        LTEST=LTEST.AND.(ERMAX.LE.TOL)
      WRITE(LOUT,'(/7X,''Largest Error was'',1P,D10.1)')ERMAX
      WRITE(LOUT,'(1X)')
      IRC=ITEST('G102',LTEST)
      CALL PAGEND('G102')
      RETURN
      END
