*
* $Id: d115m.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d115m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D115M
C     Program to test the MATHLIB routine CHEBQU (D115), which performs
C     Double-precision Clenshaw-Curtis integration.
C     The functions being integrated are stored in FUNCTION subprograms
#include "gen/imp64.inc"
C     Specify the total number of tests
      PARAMETER ( NT=3 )
C     Specify the largest error allowed in a successful test
      PARAMETER ( TSTERR=8D-10 )
      DIMENSION A(NT),B(NT)
#include "iorc.inc"
      EXTERNAL D115F1,D115F2,D115F3
      DATA A,B / -2D0, 4D0, -5D0, 5D0, 1D0, 7D0 /

      CALL HEADER('D115',0)
      ERRMAX= 0D0
      EPSIN= 1D-12
      DO 100 I=1,NT
         WRITE(LOUT,'(/'' Test Number'',I2)') I
         IF (I .EQ. 1) THEN
            SOL= CHEBQU( A(I),B(I),EPSIN,EPSOUT,0,D115F1 )
            EXACT= EXP( B(I) )-EXP( A(I) )
         ELSEIF (I .EQ. 2) THEN
            SOL= CHEBQU( A(I),B(I),EPSIN,EPSOUT,0,D115F2 )
            EXACT= SIN( B(I) )-SIN( A(I) )
         ELSE
            SOL= CHEBQU( A(I),B(I),EPSIN,EPSOUT,0,D115F3 )
            EXACT= 2*(B(I)-A(I))+(A(I)**2-B(I)**2)+(B(I)**4-A(I)**4)/4
         ENDIF
         ERROR= ABS( (SOL-EXACT)/SOL )
         ERRMAX= MAX( ERRMAX,ERROR )
         WRITE(LOUT,'('' Numerical value of Integral '',F25.16)') SOL
         WRITE(LOUT,'('' Analytical value of Integral'',F25.16)') EXACT
         WRITE(LOUT,'('' Relative Error              '',F25.16)') ERROR
100   CONTINUE
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX
      IRC= ITEST('D115',ERRMAX .LE. TSTERR)
      CALL PAGEND('D115')
      RETURN
      END
