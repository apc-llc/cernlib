*
* $Id: d108m.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d108m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D108M
C     Program to test the operation of the MATHLIB routine TRAPER (D108)
C     The routine being tested performs approximate integration
C     using the Trapezoidal Rule.
C     The test is constructed by integrating numerically, arbitrary,
C     (but easy to integrate analytically), functions.
C     The numerical approximation to the integral is compared with
C     the analytical solution and the relative error is calculated.
C     Note that the function to be integrated is not passed directly
C     to the TRAPER routine, but, function-values F(X(I)) are stored
C     in the array Y; ie Y(I)=F(X(I))
C     Specify the total number of tests, and the largest No_intervals
      PARAMETER ( NT=3,NMAX=201 )
C     Specify the largest error permitted for a successful test
      PARAMETER ( TSTERR=1E-3 )
      DIMENSION X(NMAX),Y(NMAX),EPS(NMAX),N(NT),A(NT),B(NT)
#include "iorc.inc"
C     Specify the test parameters
      DATA N(1),A(1),B(1) /  81, 0.0, 5.0 /
      DATA N(2),A(2),B(2) /  81,-3.0, 2.0 /
      DATA N(3),A(3),B(3) /  81,-2.0, 2.0 /

      CALL HEADER('D108',0)
      ERRMAX=0
      DO 200 I=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') I
         H= (B(I)-A(I))/ (N(I)-1)
         DO 100 J=1,N(I)
            X(J)= A(I)+ (J-1)*H
            EPS(J)= 0
C           Initialise the Y vector
            IF (I .EQ. 1) THEN
               Y(J)= EXP(X(J))
            ELSEIF (I .EQ. 2) THEN
               Y(J)= X(J)*X(J)
            ELSE
               Y(J)= COS(X(J))
            ENDIF
100      CONTINUE
C        Calculate the integral numerically
         CALL TRAPER(X,Y,EPS,N(I),A(I),B(I),RE,SD)
C        Calculate the integral analytically
         IF (I .EQ. 1) THEN
            EXACT= EXP(B(I))-EXP(A(I))
         ELSEIF (I .EQ. 2) THEN
            EXACT= (B(I)**3)/3 - (A(I)**3)/3
         ELSE
            EXACT= SIN(B(I))-SIN(A(I))
         ENDIF
C        Calculate the relative error of this approximation
         ERROR= ABS( (EXACT-RE)/RE )
         ERRMAX= MAX( ERRMAX,ERROR )
         WRITE(LOUT,'('' Approximation to Integral '',F22.14)') RE
         WRITE(LOUT,'('' Analytic value of Integral'',F22.14)') EXACT
         WRITE(LOUT,'('' Relative Error            '',F22.14)') ERROR
200   CONTINUE
      WRITE(LOUT,   '(/'' Largest Relative Error was'',D10.1 )') ERRMAX
      IRC= ITEST('D108',ERRMAX .LE. TSTERR)
      CALL PAGEND('D108')
      RETURN
      END
