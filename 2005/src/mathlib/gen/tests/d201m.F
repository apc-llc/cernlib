*
* $Id: d201m.F,v 1.1.1.1 1996/04/01 15:01:24 mclareni Exp $
*
* $Log: d201m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:24  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D201M
C     This program tests the MATHLIB routine DEQBS and DDEQBS (D201)

#include "gen/imp64.inc"
      EXTERNAL SD201

      DIMENSION Y(5),W(5*36),YT(8)

#include "iorc.inc"
      DATA (YT(J),J=1,8)
     +/1.1999435709D0,-0.0082168071D0,-0.014411453D0,-1.049218128D0,
     + 1.2D0,0D0,0D0,-1.04935750983D0/
      CALL HEADER('D201',0)
      ERRMAX=0D0
      TSTERR=5D-9
      EPS=1D-12
      ERR=1D-20

C     Test example  "earth - moon - spaceship"  on page 9 of
C     R. Bulirsch and J. Stoer, Numerical Treatment of Ordinary
C     Differential Equations by Extrapolation Methods,
C     Numer. Math. 8 (1966) 1 - 13

      N=4


C     Integration by repeated CALLs

      DO 1 IT = 0,123
      IF(MOD(IT,50) .EQ. 0) WRITE(LOUT,100)
      T=IT/20D0
      TE=(IT+1)/20D0
      IF(IT .EQ. 0) THEN
       Y(1)=1.2D0
       Y(2)=0
       Y(3)=0
       Y(4)=-1.04935 750983D0
       H=0.02D0
      END IF
#if defined(CERNLIB_DOUBLE)
      CALL DDEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  DEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
      WRITE(LOUT,'(1X,F6.2,4F18.12,F10.6)')
     +  TE,Y(1),Y(2),Y(3),Y(4),H
    1 CONTINUE
      ERRMAX=MAX(ABS(Y(1)-YT(1)),ABS(Y(2)-YT(2)),
     +ABS(Y(3)-YT(3)), ABS(Y(4)-YT(4)),ERRMAX)

C     Integration by one CALL

      WRITE(LOUT,'(1X)')
      T=0
      TE=6.20D0
      Y(1)=1.2D0
      Y(2)=0
      Y(3)=0
      Y(4)=-1.04935 750983D0
      H=0.2D0
#if defined(CERNLIB_DOUBLE)
      CALL DDEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  DEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
      ERRMAX=MAX(ABS(Y(1)-YT(1)),ABS(Y(2)-YT(2)),
     +ABS(Y(3)-YT(3)), ABS(Y(4)-YT(4)),ERRMAX)
      WRITE(LOUT,'(1X,F6.2,4F18.12)') TE,Y(1),Y(2),Y(3),Y(4)

C     Integration over the closed orbit by one CALL (in both directions)

      DO 2 K = 0,1
      WRITE(LOUT,'(1X)')
      IF(K .EQ. 0) THEN
       T=0
       TE=6.19216 93313 96D0
       H=0.2D0
      ELSE
       T=6.19216 93313 96D0
       TE=0
       H=-0.2D0
      END IF
      Y(1)=1.2D0
      Y(2)=0
      Y(3)=0
      Y(4)=-1.04935 750983D0
      CALL TIMED(T0)
#if defined(CERNLIB_DOUBLE)
      CALL DDEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  DEQBS(N,T,TE,Y,H,EPS,SD201,W)
#endif
      CALL TIMED(T1)
      ERRMAX=MAX(ABS(Y(1)-YT(5)),ABS(Y(2)-YT(6)),
     +ABS(Y(3)-YT(7)), ABS(Y(4)-YT(8)),ERRMAX)
      WRITE(LOUT,'(1X,F10.4)') T1
    2 WRITE(LOUT,'(1X,5F18.12)') TE,Y(1),Y(2),Y(3),Y(4)

      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERRMAX

C     Test error condition

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
* DEQBS just returns, and does NOT print any error message!
#if defined(CERNLIB_DOUBLE)
*      CALL DDEQBS(0,T,TE,Y,H,ERR,SD201,W)
#endif
#if !defined(CERNLIB_DOUBLE)
*      CALL  DEQBS(0,T,TE,Y,H,ERR,SD201,W)
#endif

      WRITE(LOUT,'(1X)')
      T=0
      TE=0.01D0
      Y(1)=1.2D0
      Y(2)=0
      Y(3)=0
      Y(4)=-1.04935 750983D0
      H=0.2D0
#if defined(CERNLIB_DOUBLE)
      CALL DDEQBS(N,T,TE,Y,H,ERR,SD201,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  DEQBS(N,T,TE,Y,H,ERR,SD201,W)
#endif

  100 FORMAT('1',/,5X,'T',14X,'X(T)',14X,'Y(T)',13X,'X.(T)',13X,
     1           'Y.(T)',9X,'H'/)
C     Check if the test was completed successfully
      IRC=ITEST('D201',ERRMAX .LE. TSTERR)
      CALL PAGEND('D201')
      RETURN
      END
      SUBROUTINE SD201(X,Y,F)
#include "gen/imp64.inc"
      DIMENSION Y(*),F(*)

      PARAMETER (FMU = 1/82.45D0, FM1 = 1-FMU)

      X=X+0
      HP=1/SQRT(((Y(1)+FMU)**2+Y(2)**2)**3)
      HM=1/SQRT(((Y(1)-FM1)**2+Y(2)**2)**3)
      F(1)=Y(3)
      F(2)=Y(4)
      F(3)=Y(1)+2*Y(4)-FM1*HP*(Y(1)+FMU)-FMU*HM*(Y(1)-FM1)
      F(4)=Y(2)-2*Y(3)-FM1*HP*Y(2)-FMU*HM*Y(2)
      RETURN
      END
