*
* $Id: d200m.F,v 1.1.1.1 1996/04/01 15:01:24 mclareni Exp $
*
* $Log: d200m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:24  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D200M
C     This program tests the operation of MATHLIB subprograms
C     RKSTP and DRKSTP (D(200)

#include "gen/imp64.inc"
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-10)

      EXTERNAL D200S1

      DIMENSION Y(2),W(2,3)

      PARAMETER (Z0 = 0, Z1 = 1)
#include "iorc.inc"

      CALL HEADER('D200',0)
      ERMAX= 0D0

      WRITE(LOUT,'(/8X,''X'',12X,''Y(1)'',9X,''ErrorY1'',11X,''Y(2)'',
     +              8X,''ErrorY2'')')
      X=Z1
#if defined(CERNLIB_DOUBLE)
      Y(1)=DBESJ0(X)
      Y(2)=-DBESJ1(X)
#endif
#if !defined(CERNLIB_DOUBLE)
      Y(1)= BESJ0(X)
      Y(2)=- BESJ1(X)
#endif
      Y1=Y(1)
      Y2=Y(2)
      WRITE(LOUT,'(1X,F10.2,F20.15,D10.1,F20.15,D10.1)') X,Y(1),Y(1)-Y1,
     +                                                     Y(2),Y(2)-Y2
      DO 1 J = 0,199
#if defined(CERNLIB_DOUBLE)
      CALL DRKSTP(2,Z1/100,X,Y,D200S1,W)
      Y1=DBESJ0(X)
      Y2=-DBESJ1(X)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  RKSTP(2,Z1/100,X,Y,D200S1,W)
      Y1= BESJ0(X)
      Y2=- BESJ1(X)
#endif
      ER1=ABS(Y(1)-Y1)
      ER2=ABS(Y(2)-Y2)
      ERMAX=MAX(ERMAX,ER1,ER2)
      IF(J .EQ. 0 .OR. J .EQ. 49 .OR. J .EQ. 99 .OR. J .EQ. 199)
     +WRITE(LOUT,'(1X,F10.2,F20.15,D10.1,F20.15,D10.1)') X,Y(1),ER1,
     +                                                     Y(2),ER2
    1 CONTINUE
      WRITE(LOUT,'(/'' Largest Error was'',D10.1)') ERMAX
      IRC= ITEST('D200',ERMAX .LE. TSTERR)
      CALL PAGEND('D200')
      RETURN
      END
      SUBROUTINE D200S1(X,Y,F)

#include "gen/imp64.inc"
      DIMENSION Y(*),F(*)

      F(1)=Y(2)
      F(2)=-Y(2)/X-Y(1)
      RETURN
      END
