*
* $Id: b101m.F,v 1.1.1.1 1996/04/01 15:01:12 mclareni Exp $
*
* $Log: b101m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:12  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE B101M
C     Routine to test MATHLIB routine ATG (B101)

C     Specify the largest error allowed for a successful test
      PARAMETER ( TSTERR=1D-6 )
#include "iorc.inc"
      CALL HEADER('B101',0)

      WRITE(LOUT,'(/8X,''For A=I*PI/180:'')')
      WRITE(LOUT,'(/5X,''I'',3X,''X=SIN(A)'',4X,''Y=COS(A)'',8X,
     +                 ''ATG(X,Y)'',4X,''Error'')')
      ER=0E0
      ERMAX=0E0
      PI = 3.14159 26535 89793D0
      DO 1 I = 0,350,10
      A=I*PI/180
      X1=SIN(A)
      X2=COS(A)
      R1=ATG(X1,X2)
      IF (A .NE. 0.)ER=ABS((R1-A)/A)
      ERMAX=MAX(ERMAX,ER)
      WRITE(LOUT,'(1X,I5,2F12.7,F15.7,E10.1)') I,X1,X2,R1,ER
    1 CONTINUE
      WRITE(LOUT,'(/7X,''Largest Error for ATG was'',E10.1)')ERMAX
      IRC=ITEST('B101',ERMAX .LE. TSTERR)
      CALL PAGEND('B101')
      RETURN
      END
