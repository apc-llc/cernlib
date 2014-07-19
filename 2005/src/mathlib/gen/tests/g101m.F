*
* $Id: g101m.F,v 1.1.1.1 1996/04/01 15:01:27 mclareni Exp $
*
* $Log: g101m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:27  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G101M
C     Routine to test MATHLIB routine CHISIN (G101)

#include "gen/imp64.inc"

      REAL CHISIN,P,E,R,T,ERMAX
      LOGICAL LTEST
      EXTERNAL FZR
      COMMON /FORFZR/ DPP
      COMMON /FIG101/ FNORM,EX
      COMMON /NNG101/ N
#include "iorc.inc"
      PARAMETER (Z0 = 0, Z1 = 1, Z2 = 2, HF = Z1/2)

C     Set numerical tolerance for test to be successful
      DATA TOL / 1E-3/

      CALL HEADER('G101',0)
      LTEST=.TRUE.
      ERMAX=0E0
      WRITE (LOUT,'(/5x,''N'',8X,''P'',9x,''CHISIN'',9X,
     +''TEST'',7X,''Error'')')
      DO 1 N = 1,10
      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      FNORM=1/(Z2**(HF*N)*DGAMMA(HF*N))
#endif
#if !defined(CERNLIB_DOUBLE)
      FNORM=1/(Z2**(HF*N)* GAMMA(HF*N))
#endif
      EX=HF*N-1
      DO 1 IP = 0,9
      DPP=IP/10D0
      P=DPP
      R=CHISIN(P,N)
      DR=R
      T=0
#if defined(CERNLIB_DOUBLE)
      IF(P .GT. 0) T=DZEROX(-0.015+DR,DR+0.015,5D-8,500,FZR,1)
#endif
#if !defined(CERNLIB_DOUBLE)
      IF(P .GT. 0) T= ZEROX(-0.015+DR,DR+0.015,5D-8,500,FZR,1)
#endif
      E=0
      IF(R .NE. 0) E=ABS((R-T)/R)
      ERMAX=MAX(ERMAX,E)
    1 WRITE(LOUT,'(1X,I5,F10.1,2F15.7,1P,D10.1)') N,P,R,T,E
        LTEST=LTEST.AND.(ERMAX.LE.TOL)
      WRITE(LOUT,'(/7X,''Largest Error was'',1P,D10.1)')ERMAX
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
      R=CHISIN(-1.,1)
      R=CHISIN(+2.,1)
      R=CHISIN(+0.5,-1)
      WRITE(LOUT,'(1X)')
      IRC=ITEST('G101',LTEST)
      CALL PAGEND('G101')
      RETURN
      END
      FUNCTION FZR(X)
#include "gen/imp64.inc"
      COMMON /FORFZR/ DPP
      COMMON /NNG101/ N
      EXTERNAL FG101

      IF(N .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
       FZR=DERF(SQRT(X/2))-DPP
#endif
#if !defined(CERNLIB_DOUBLE)
       FZR= ERF(SQRT(X/2))-DPP
#endif
      ELSEIF(N .EQ. 2) THEN
       FZR=1-EXP(-X/2)-DPP
      ELSE
#if defined(CERNLIB_DOUBLE)
       FZR=DGAUSS(FG101,0D0,X,1D-10)-DPP
#endif
#if !defined(CERNLIB_DOUBLE)
       FZR= GAUSS(FG101,0D0,X,1D-10)-DPP
#endif
      ENDIF
      RETURN
      END
      FUNCTION FG101(T)
#include "gen/imp64.inc"
      COMMON /FIG101/ FNORM,EX

      FG101=FNORM*T**EX*EXP(-0.5D0*T)
      RETURN
      END
