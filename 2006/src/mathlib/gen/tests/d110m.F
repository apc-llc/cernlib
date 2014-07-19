*
* $Id: d110m.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110M
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X(6)
      PARAMETER ( TSTERR=1D-13 )
      EXTERNAL D110S1,D110S3
C
C     First test is example in Short write up, in Library manual
C          =1                   =Sqrt(X2)
C         |                    |
C     I=  |  Sqrt(X2)*Exp(X2)  |  X1*Sqrt{(X1^2)+X2}  dX1  dX2
C         |                    |
C       0=                   0=
C
      WRITE(LOUT,'(/'' Test Number 1: Double integral'')')
C     Calculate the numerical value of the integrals
#if defined(CERNLIB_DOUBLE)
      SOL1=DGMLT2(D110S1,0D0,1D0,3,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
      SOL1=RGMLT2(D110S1,0.0,1.0,3,6,X)
#endif
      EXACT1= (2*SQRT(2D0)-1D0)*(EXP(1D0)-2D0)/3
      ERROR1=ABS( (SOL1-EXACT1)/SOL1 )
      WRITE(LOUT,'('' Calculated Integral'',F25.16)') SOL1
      WRITE(LOUT,'('' Exact Integral     '',F25.16)') EXACT1
      WRITE(LOUT,'('' Relative Error     '',F25.16)') ERROR1
C
C     Second test example constructed by author:
C         =a   =X1  =X2  =X3  =X4  =X5
C        |    |    |    |    |    |
C     I= |    |    |    |    |    |  X1+X2+X3+X4+X5+X6   dX6 dX5 dX4...
C        |    |    |    |    |    |                      ...dX3 dX2 dX1
C      0=   0=   0=   0=   0=   0=
C
      WRITE(LOUT,'(/'' Test Number 2: Six-dimensional integral'')')
      A=1D0
#if defined(CERNLIB_DOUBLE)
      SOL2=DGMLT6(D110S3,0D0,A,2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
      SOL2=RGMLT6(D110S3,0.0,A,2,6,X)
#endif
      EXACT2= (A**7)/240
      ERROR2=ABS((EXACT2-SOL2)/SOL2)
      WRITE(LOUT,'('' Calculated Integral'',F25.16)') SOL2
      WRITE(LOUT,'('' Exact Integral     '',F25.16)') EXACT2
      WRITE(LOUT,'('' Relative Error     '',F25.16)') ERROR2

C     Calculate largest error
      ERRMAX=MAX( ERROR1,ERROR2 )
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX
      IRC=ITEST('D110',ERRMAX .LE. TSTERR)
      CALL PAGEND('D110')
      RETURN
      END

C     ****** Nested function evaluation for test number 1 ******
