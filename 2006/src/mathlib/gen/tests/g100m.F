*
* $Id: g100m.F,v 1.1.1.1 1996/04/01 15:01:27 mclareni Exp $
*
* $Log: g100m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:27  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G100M
C     Routine to test MATHLIB routine PROB (G100)

#include "gen/imp64.inc"
      REAL PROB,CH2,R,H,E,ERMAX
      LOGICAL LTEST
      PARAMETER (NMAX = 6)
c     DIMENSION NN(NMAX),ZU(50),ZG(50)
      DIMENSION NN(NMAX)

      PARAMETER (R0 = 0, R1 = 1, R2 = 2, HF = R1/2, TH = R1/3)
      PARAMETER (F1 = 2*R1/9)

#include "iorc.inc"

      DATA NN /1,5,10,50,75,110/
C     Set numerical tolerance for test to be successful
      DATA TOL / 1E-3/

#if defined(CERNLIB_DOUBLE)
      FQ(U)=DERFC(U/SQRT(R2))/2
#endif
#if !defined(CERNLIB_DOUBLE)
      FQ(U)=ERFC(U/SQRT(R2))/2
#endif
      FZ(U)=EXP(-U**2/2)/SQRT(2*PI)
      CALL HEADER('G100',0)
      LTEST=.TRUE.

      WRITE (LOUT,'(/5X,''X'',7X,''DF'',11X,''PROB'',16X,
     +''TEST'',9X,''Error'')')
      ERMAX=0E0
      PI = 3.14159 26535 89793D0
      DO 11 N1 = 1,NMAX
      WRITE(LOUT,'(1X)')
      N=NN(N1)
      DO 12 ICH2 = 0,50
      DCH2=ICH2
      IF(N .GT. 70) DCH2=2*DCH2
      X=DCH2/2
      CH2=DCH2
      R=PROB(CH2,N)
      IF(N .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
       H=DERFC(SQRT(X))
#endif
#if !defined(CERNLIB_DOUBLE)
       H= ERFC(SQRT(X))
#endif
      ELSE
       S=1
       T=1
       M=N/2
       IF(2*M .EQ. N) THEN
        DO 1 I = 1,M-1
        T=X*T/I
    1   S=S+T
        H=SQRT(2*PI)*FZ(SQRT(DCH2))*S
       ELSE
        DO 2 I=1,M-1
        T=T*DCH2/(2*I+1)
    2   S=S+T
        W=SQRT(DCH2)
        H=2*FQ(W)+2*FZ(W)*W*S
       ENDIF
      ENDIF
      IF (R .GT. 1E-3)THEN
      E=ABS((H-R)/R)
      ELSE
      E=ABS(H-R)
      ENDIF
      ERMAX=MAX(ERMAX,E)
      WRITE(LOUT,'(1X,I5,F10.1,1P,2E20.8,E10.1)') N,CH2,R,H,E
      IF(R .LT. 5D-8) GO TO 11
   12 CONTINUE
   11 CONTINUE
        LTEST=LTEST.AND.(ERMAX.LE.TOL)
      WRITE(LOUT,'(/7X,''Largest Error was'',1P,E10.1)')ERMAX
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
      R=PROB(2E0,0)
      R=PROB(2E0,-1)
      R=PROB(-2E0,1)
      WRITE(LOUT,'(1X)')
      IRC=ITEST('G100',LTEST)
      CALL PAGEND('G100')
      RETURN
      END
