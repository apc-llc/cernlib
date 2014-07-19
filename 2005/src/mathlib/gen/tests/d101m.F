*
* $Id: d101m.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d101m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D101M
#include "iorc.inc"
C     Program to test the GENLIB routines SIMPS and DSIMPS (D101) by
C     using the routines to numerically integrate known integrals
C             Written by T Hepworth, 8th May 1990
C     B. Damgaard : introduce new sequences and test error msg (10/92)
C     Set maximum number of steps and number of tests
      PARAMETER ( NMAX=100,NT=4 )
#include "gen/def64.inc"
     +      F(0:NMAX),A,B,X,DSIMPS,TSTERR,
     +      START(NT),FINISH(NT),H(NT),
     +      RESULT(NT),SOL(NT),ERROR(NT),ERRMAX,
     +      DF(0:10),Z1,Z2
      INTEGER          N(NT)
C     Set the maximum percentage relative error allowed for the test
C     to still be considered successful
      PARAMETER ( TSTERR=1D-3 )
      PARAMETER (Z1 = 1, Z2 = 2)
C     The test data
      DATA START(1),FINISH(1),N(1) /   0D0,  3D0,  50 /
      DATA START(2),FINISH(2),N(2) /  -6D0,  6D0, 100 /
      DATA START(3),FINISH(3),N(3) /  -1D0,  1D0,  20 /
      DATA START(4),FINISH(4),N(4) /   8D0, 28D0, 100 /

      CALL HEADER('D101',0)
      DO 200 I=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') I
C        Calculate step length H for test number I
         H(I)= ( FINISH(I)-START(I) )/ N(I)
         WRITE(LOUT,'('' Integration Interval ('',F4.1,'','',F4.1,'')'',
     +         3X,''Step length'',F5.2,3X,''Number of steps'',I4)')
     +                                   START(I),FINISH(I),H(I),N(I)
C        Initialise the function to be integrated (at discrete points)
         DO 100 J=0,N(I)
C           Calculate Xj
            X= START(I)+( J*H(I) )
            IF (I .EQ. 1) THEN
               F(J)= EXP(2D0*X)
            ELSEIF (I .EQ. 2) THEN
               F(J)= EXP(-X)
            ELSEIF (I .EQ. 3) THEN
               F(J)= COS(X)
            ELSE
               F(J)= X*SIN(X)
            ENDIF
100      CONTINUE

C        Compute the numerical approximation to the integral
         A= START(I)
         B= FINISH(I)
#if defined(CERNLIB_DOUBLE)
         RESULT(I)= DSIMPS(F,A,B,N(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         RESULT(I)= SIMPS(F,A,B,N(I))
#endif
C        Compute the analytical value of the integral
         IF (I .EQ. 1) THEN
            SOL(I)= ( EXP(2D0*B)-EXP(2D0*A) )/ 2D0
         ELSEIF (I .EQ. 2) THEN
            SOL(I)= ( EXP(-A)-EXP(-B) )
         ELSEIF (I .EQ. 3) THEN
            SOL(I)= SIN(B)-SIN(A)
         ELSE
            SOL(I)= (A*COS(A))-(B*COS(B))+SIN(B)-SIN(A)
         ENDIF

C        Compute Relative error as a positive percentage
         ERROR(I)= ABS( (RESULT(I)-SOL(I))/RESULT(I) )*1D2
         WRITE(LOUT,'('' Numerical approximation'',F25.16)') RESULT(I)
         WRITE(LOUT,'('' Analytical value       '',F25.16)') SOL(I)
         WRITE(LOUT,'('' Relative Error         '',F23.14,''%'')')
     +                                                  ERROR(I)
200   CONTINUE
C     Compute maximum (percentage) relative error
      ERRMAX= MAX( ERROR(1),ERROR(2),ERROR(3),ERROR(4) )
      WRITE(LOUT,'(/'' Largest Relative Error was'',F23.14,''%'')')
     +                                                 ERRMAX
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      R=DSIMPS(DF,-Z1,Z2,1)
      R=DSIMPS(DF,-Z1,Z2,-3)
#endif
#if !defined(CERNLIB_DOUBLE)
      R= SIMPS(DF,-Z1,Z2,1)
      R= SIMPS(DF,-Z1,Z2,-3)
#endif
C     Check if the test was successful
      IRC=ITEST('D101',ERRMAX .LE. TSTERR)
      CALL PAGEND('D101')
      RETURN
      END
