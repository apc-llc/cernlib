*
* $Id: c200m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c200m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C200M
C     Test of MATHLIB routines ZEROX and DZEROX (C200)
#include "gen/imp64.inc"
C     Specify the largest absolute error allowed for a successful test
      PARAMETER        ( TSTERR=5D-11 )
      EXTERNAL         C200F1,C200F2,C200F3,C200F4
      CHARACTER CMR(2)*1
      COMMON /ICOMMN/ KASE,N
      COMMON /RCOMMN/ A,B
#include "iorc.inc"

      DIMENSION A2(3),B2(3)
      DIMENSION N1(4,6),N2(4),N3(6)

      DATA CMR /'M','R'/
      DATA (N1(N,1),N=1,4) / 1, 0, 0, 0/
      DATA (N1(N,2),N=1,4) / 1, 2, 3, 4/
      DATA (N1(N,3),N=1,4) / 1, 5,10, 0/
      DATA (N1(N,4),N=1,4) / 1, 5,10, 0/
      DATA (N1(N,5),N=1,4) / 1, 4, 8, 0/
      DATA (N1(N,6),N=1,4) / 1, 5,10, 0/
      DATA N2 / 3, 5, 9,19/, N3 / 3, 5, 7, 9,19,25/
      DATA A2 / 1, 0, 1/, B2 / 0, 1D-4,1D-4/

      CALL HEADER('C200',0)
#if !defined(CERNLIB_DOUBLE)
      TOL=1D-14
#endif
#if defined(CERNLIB_DOUBLE)
      TOL=1D-15
#endif
      MXF=500
      ERMAX=0D0
      DO 20 MODE = 1,2
      WRITE(LOUT,100) MODE,CMR(MODE)
      WRITE(LOUT,'(/3X,''F'',2X,''N'',10X,''X0'',17X,''F(X0)'')')
      DO 1 KASE = 1,6
      WRITE(LOUT,'(1X)')
      XL=0
      XU=1
      IF(KASE .EQ. 1) XU=1.5D0
      DO 11 I1 = 1,4
      N=N1(I1,KASE)
      IF(N .EQ. 0) GO TO 11
#if defined(CERNLIB_DOUBLE)
      X0=DZEROX(XL,XU,TOL,MXF,C200F1,MODE)
#endif
#if !defined(CERNLIB_DOUBLE)
      X0= ZEROX(XL,XU,TOL,MXF,C200F1,MODE)
#endif
      ER=ABS(C200F1(X0))
      WRITE(LOUT,'(1X,2I3,2F20.14)') KASE,N,X0,C200F1(X0)
      ERMAX=MAX(ERMAX,ER)
   11 CONTINUE
    1 CONTINUE

      WRITE(LOUT,'(/6X,''N'',6X,''A'',8X,''B'',18X,''X0'',27X,
     +''F(X0)'')')
      XL=-1
      XU=1
      DO 2 I2 = 1,2
C     DO 2 I2 = 1,4
      IF ( MODE .EQ. 1 .AND. I2 .GE. 3) GO TO 2
      N=N2(I2)
      DO 12 J2 = 1,3
      A=A2(J2)
      B=B2(J2)
#if defined(CERNLIB_DOUBLE)
      X0=DZEROX(XL,XU,TOL,MXF,C200F2,MODE)
#endif
#if !defined(CERNLIB_DOUBLE)
      X0= ZEROX(XL,XU,TOL,MXF,C200F2,MODE)
#endif
      ER=ABS(C200F2(X0))
      WRITE(LOUT,'(1X,I6,2F10.4,2F25.15)') N,A,B,X0,C200F2(X0)
      ERMAX=MAX(ERMAX,ER)
   12 CONTINUE
    2 CONTINUE

C     WRITE(LOUT,'(/6X,''N'',14X,''X0'',22X,''F(X0)'')')
C     XL=-1
C     XU=10
C     DO 3 I3 = 1,6
C     N=N3(I3)
CSELF,IF= DOUBLE
C     X0=DZEROX(XL,XU,TOL,MXF,C200F3,MODE)
CSELF,IF=-DOUBLE
C     X0= ZEROX(XL,XU,TOL,MXF,C200F3,MODE)
CSELF.
C     ER=ABS(C200F3(X0))
C     WRITE(LOUT,'(1X,I6,2F25.15)') N,X0,C200F3(X0)
C     ERMAX=MAX(ERMAX,ER)
C   3 CONTINUE

      WRITE(LOUT,'(/19X,''X0'',22X,''F(X0)'')')
      XL=-1
      XU=4
#if defined(CERNLIB_DOUBLE)
      X0=DZEROX(XL,XU,TOL,MXF,C200F4,MODE)
#endif
#if !defined(CERNLIB_DOUBLE)
      X0= ZEROX(XL,XU,TOL,MXF,C200F4,MODE)
#endif
      ER=ABS(C200F4(X0))
      WRITE(LOUT,'(1X,6X,2F25.15)') X0,C200F4(X0)
      ERMAX=MAX(ERMAX,ER)
   20 CONTINUE
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      X0=DZEROX(XL,XU,TOL,MXF,C200F4,0)
      X0=DZEROX(XL,XL,TOL,MXF,C200F4,1)
      X0=DZEROX(XL,XU,TOL,2,C200F4,1)
#endif
#if !defined(CERNLIB_DOUBLE)
      X0= ZEROX(XL,XU,TOL,MXF,C200F4,0)
      X0= ZEROX(XL,XL,TOL,MXF,C200F4,1)
      X0= ZEROX(XL,XU,TOL,2,C200F4,1)
#endif
  100 FORMAT('1'/1X,'MODE = ',I1,5X,'Algorithm ',A1,
     1 5X,'(Examples on page 340 of Bus and Dekker'/)
        WRITE(LOUT,'(/''Largest Relative Error was'',1P,D10.1)')ERMAX
      IRC=ITEST('C200',ERMAX .LE. TSTERR)
      CALL PAGEND('C200')
      RETURN
      END
C     Test functions follow, equation to be solved: F(x)=0 for a<=x<=b
      FUNCTION C200F(X)
#include "gen/imp64.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION C200F1,C200F2,C200F3,C200F4,X
#endif
      COMMON /ICOMMN/ KASE,N
      COMMON /RCOMMN/ A,B

      PARAMETER (Z1 = 1, HALF = Z1/2)

      ENTRY C200F1(X)
      GO TO (1,2,3,4,5,6), KASE
    1 C200F=SIN(X)-HALF
      RETURN
    2 C200F=2*X*EXP(-N*Z1)+1-2*EXP(-N*X)
      RETURN
    3 C200F=(1+(1-N)**2)*X-(1-N*X)**2
      RETURN
    4 C200F=X**2-(1-X)**N
      RETURN
    5 C200F=(1+(1-N)**4)*X-(1-N*X)**4
      RETURN
    6 C200F=(X-1)*EXP(-N*X)+X**N
      RETURN

      ENTRY C200F2(X)
      C200F=X**N+A*X+B
      RETURN

      ENTRY C200F3(X)
      C200F=X**N
      RETURN

      ENTRY C200F4(X)
      C200F=(3*X-1)*(3*X/2-1)**4*EXP(-5*X)
      RETURN
      END

