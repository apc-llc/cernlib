*
* $Id: g104m.F,v 1.1.1.1 1996/04/01 15:01:29 mclareni Exp $
*
* $Log: g104m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:29  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G104M
C     Test of MATHLIB routines STUDIS and STUDIN (G104)
C     Routine STUDIS calculates t-table values P1, for parameters T1 say,
C     the test of STUDIS is constructed by comparing this values with
C     exact t-table values obtained from using high-precision
C     Gaussian quadrature.
C     STUDIN calculates for a specified probability P say, the value
C     of the test statistic t, T2 say, that gives the specified
C     probability under a t-distribution. The test of STUDIN is
C     simply to compare T2 with T1.
C     Note that the program library manual quotes 0 as the lower limit
C     of the t-integral, this should be minus infinity.

C     Specify the total number of tests present
      PARAMETER ( NT=5 )
      REAL T1,T2,P,P1,ERROR,ERRMX1,ERRMX2,ERRMAX,STUDIS,STUDIN,TSTERR
C     Specify the largest relative error permitted in a successful test
      PARAMETER ( TSTERR=1E-4 )
      DIMENSION P(NT),T1(NT)
      INTEGER   N(NT)
#include "iorc.inc"
      DATA T1 / 0.1, 0.2, 0.5, 0.9, 3.0 /
      DATA P / 0.5317255174305536,0.5700140042014005,
     +   0.6742760175759245,0.7904972403959577,0.9849503760512686 /
      DATA N / 1,2,3,4,5 /

      CALL HEADER('G104',0)
C     Testing routine STUDIS
      ERRMX1= 0D0
      WRITE(LOUT,'(/4X,''t'',3X,''n'',8X,''STUDIS(t,n)'',8X,
     +         ''Exact Value'',5X,''Relative Error'')')
      DO 100 I=1,NT
         P1= STUDIS( T1(I),N(I) )
         ERROR= ABS( (P(I)-P1)/P1 )
         WRITE(LOUT,'(F5.1,3X,I1,3F19.12)') T1(I),N(I),P1,P(I),ERROR
         ERRMX1= MAX( ERRMX1,ERROR )
100   CONTINUE

C     Testing routine STUDIN
      ERRMX2= 0D0
      WRITE(LOUT,'(/15X,''P'',3X,''n'',8X,''STUDIN(P,n)  Exact'',5X,
     +              ''Relative Error'')')
      DO 200 I=1,NT
         T2= STUDIN( P(I),N(I) )
         ERROR= ABS( (T1(I)-T2)/T2 )
         WRITE(LOUT,'(F16.12,3X,I1,F19.12,F7.1,F19.12)') P(I),N(I),
     +                T2,T1(I),ERROR
         ERRMX2= MAX( ERRMX2,ERROR )
200   CONTINUE

      ERRMAX=MAX( ERRMX1,ERRMX2 )
      WRITE(LOUT,'(/'' Largest relative Error was'',F19.12)') ERRMAX
      IRC= ITEST( 'G104',ERRMAX .LE. TSTERR )
      CALL PAGEND('G104')
      RETURN
      END
