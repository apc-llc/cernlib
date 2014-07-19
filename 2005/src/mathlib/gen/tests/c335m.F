*
* $Id: c335m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c335m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C335M
C     This program tests the operation of MATHLIB routines
c     CWERF and WWERF (C335)
#include "gen/defc64.inc"
     +           I,Z,ZT(5)
      CHARACTER NAME*6
      LOGICAL LTEST
      COMMON /C335T/ LTEST,ERRMAX,SERRMAX

#include "iorc.inc"
      DATA I /(0  ,1  )/

      CALL HEADER('C335',0)
      ERRMAX=0D0
      SERRMAX=0E0
      LTEST= .TRUE.
      ZT(1)=0.44D0+0.67D0*I
      ZT(2)=0.44D0+0.61D0*I
      ZT(3)=0.39D0+0.61D0*I
      ZT(4)=0.4D0-1.3D0*I
      ZT(5)=7D0+2D0*I

C---  Number of functions to test
#if defined(CERNLIB_DOUBLE)
      KP=1
#endif
#if !defined(CERNLIB_DOUBLE)
      KP=2
#endif
      DO 9 IDS = KP,2
      IF(IDS .EQ. 1) NAME='WWERF '
      IF(IDS .EQ. 2) NAME='CWERF '
      WRITE(LOUT,101)
      DO 1 N = 1,5
      Z=ZT(N)
      CALL C335TEST(NAME,Z,1)
    1 CONTINUE

      WRITE(LOUT,102)
      DO 2 J = 1,2
      DO 2 K = -1,1
      IF(NAME .EQ. 'WWERF ') THEN
       IF(J .EQ. 1) Z=0.01D0+(7.4D0+0.01D0*K)*I
       IF(J .EQ. 2) Z=8.3D0+0.01D0*K+0.01D0*I
      ELSE
       IF(J .EQ. 1) Z=0.01D0+(3.2D0+0.01D0*K)*I
       IF(J .EQ. 2) Z=4.5D0+0.01D0*K+0.01D0*I
      END IF
      CALL C335TEST(NAME,Z,1)
    2 CONTINUE

      WRITE(LOUT,103)
      DO 3 J = -1,1,2
      DO 3 K = -1,1,2
      Z=J*1.9D0+I*K*1.4D0
      CALL C335TEST(NAME,Z,1)
    3 CONTINUE

      WRITE(LOUT,104)
      DO 4 N = -16,92,2
      IF(N .EQ. 0) GO TO 4
      Z=N*(1+I)
      CALL C335TEST(NAME,Z,1)
    4 CONTINUE

      WRITE(LOUT,105)
#if defined(CERNLIB_QIEEE)||defined(CERNLIB_VAX)
      DO 5 N = -8,92,2
#endif
#if (!defined(CERNLIB_QIEEE))&&(!defined(CERNLIB_VAX))
      DO 5 N = -12,92,2
#endif
      IF(N .EQ. 0) GO TO 5
      Z=I*N
      CALL C335TEST(NAME,Z,1)
    5 CONTINUE

      WRITE(LOUT,106)
c     DO 6 N = -12,54,2  ******* underflow on VM
#if defined(CERNLIB_QIEEE)||defined(CERNLIB_VAX)
      DO 6 N = -8,8,2
#endif
#if (!defined(CERNLIB_QIEEE))&&(!defined(CERNLIB_VAX))
      DO 6 N = -12,12,2
#endif
      Z=N
      CALL C335TEST(NAME,Z,2)
    6 CONTINUE

      WRITE(LOUT,107)
      DO 7 IX = 1,10
      DO 7 IY = 1,10
      Z=IX+I*IY
      CALL C335TEST(NAME,Z,1)
    7 CONTINUE
    9 CONTINUE

  101 FORMAT('1'/1X,9X,'EXAMPLES 13 - 16 IN ABRAMOWITZ - STEGUN'/)
  102 FORMAT(///1X,9X,'TEST NEAR SEPARATION LINE IN ALGORITHM'/)
  103 FORMAT(///1X,9X,'TEST IN THE FOUR QUADRANTS'/)
  104 FORMAT('1'/1X,9X,'TEST ALONG THE DIAGONAL X = Y'/)
  105 FORMAT('1'/1X,9X,'TEST ALONG THE IMAGINARY AXIS X = 0'/)
  106 FORMAT('1'/1X,9X,'TEST ALONG THE REAL AXIS Y = 0'/)
  107 FORMAT('1'/1X,9X,'X',9X,'Y',10X,'W(Z)'/)
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)')SERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif
      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('C335',LTEST)
      CALL PAGEND('C335')
      RETURN
      END

      SUBROUTINE C335TEST(NAME,Z,K)
#include "gen/defc64.inc"
     +           WWERF
#include "gen/defc64.inc"
     +           Z,I,R,T,TSTERF
      LOGICAL LTEST
      COMMON /C335T/ LTEST,ERRMAX,SERRMAX
      COMPLEX CWERF,ZS
      CHARACTER NAME*6
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=7D-13)
      PARAMETER (STSTERR=7D-6 )

#include "iorc.inc"
      DATA I /(0,1)/

      ZS=Z
      IF(NAME .EQ. 'WWERF ') THEN
       R=WWERF(Z)
       T=TSTERF(Z,R)
       IF(K .EQ. 2) THEN
c       IF(ABS(-Z**2) .LT. 170D0)THEN
        R=R-EXP(-Z**2)
c       ELSE
c       R=R
c       ENDIF
        T=I*T
       END IF
       ERR=0
       IF(R .NE. 0 .AND. ABS(R) .GE. 5D-14)THEN
        ERR=ABS((T-R)/R)
       ELSE
        ERR=ABS(T-R)
       ENDIF
      ERRMAX=MAX(ERRMAX,ERR)
      LTEST= LTEST .AND. ERRMAX .LE.  TSTERR
       WRITE(LOUT,'(2F10.2,2D25.16,1P,D10.1)') ZS,R,ERR
      ELSE
       R=CWERF(ZS)
       T=TSTERF(Z,R)
       IF(K .EQ. 2) THEN
c       IF(ABS(-ZS**2) .LT. 170D0)THEN
        R=R-EXP(-ZS**2)
c       ELSE
c       R=R
c       ENDIF
        T=I*T
       END IF
       ERR=0
       IF(R .NE. 0) ERR=ABS((T-R)/R)
      SERRMAX=MAX(SERRMAX,ERR)
#if defined(CERNLIB_DOUBLE)
      LTEST= LTEST .AND. SERRMAX .LE.  STSTERR
       WRITE(LOUT,'(2F10.2,2D25.8,1P,D10.1)') ZS,R,ERR
#endif
#if !defined(CERNLIB_DOUBLE)
      LTEST= LTEST .AND. SERRMAX .LE.   TSTERR
       WRITE(LOUT,'(2F10.2,2D25.16,1P,D10.1)') ZS,R,ERR
#endif
      END IF
      RETURN
      END
      FUNCTION TSTERF(Z,R)
C     COMPLEX FUNCTION TSTERF*16(Z,R)
      EXTERNAL FR,FI,FRX
      COMMON /C335FORINT/ XS,XSYS,XY2,XY2S
#include "gen/defc64.inc"
     +           TSTERF,Z,R,U,I
#include "gen/def64.inc"
     +           XS,XSYS,XY2,XY2S,
     +           X,Y,T,PI,EPS,FR,FI,FRX
#include "gen/def64.inc"
     +           DGAUSS
#include "gen/gcmpfun.inc"
      DATA I /(0,1)/, EPS /1D-14/
      DATA PI /3.14159 26535 89793 24D0/

      U=Z
      T=GIMAG(U)
      IF(T .LT. 0D0) U=-U
      X=GREAL(U)
      Y=GIMAG(U)
      IF(Y .NE. 0D0) THEN
       XSYS=X**2-Y**2
       XY2=2*X*Y
       XY2S=XY2**2
#if defined(CERNLIB_DOUBLE)
       TSTERF=(2*I*U/PI)*(DGAUSS(FR,0D0,8D0,EPS*ABS(R))
     1                 +I*DGAUSS(FI,0D0,8D0,EPS*ABS(R)))
#endif
#if !defined(CERNLIB_DOUBLE)
       TSTERF=(2*I*U/PI)*( GAUSS(FR,0E0,8E0,EPS*ABS(R))
     1                 +I* GAUSS(FI,0E0,8E0,EPS*ABS(R)))
#endif
       IF(T .LT. 0D0) TSTERF=2*EXP(-U**2)-TSTERF
      ELSE
       XS=X**2
#if defined(CERNLIB_DOUBLE)
       TSTERF=(2/SQRT(PI))*DGAUSS(FRX,0D0,X,EPS*ABS(R))
#endif
#if !defined(CERNLIB_DOUBLE)
       TSTERF=(2/SQRT(PI))* GAUSS(FRX,0D0,X,EPS*ABS(R))
#endif
      END IF
      RETURN
      END
      FUNCTION FRI(T)
#include "gen/imp64.inc"
      COMMON /C335FORINT/ XS,XSYS,XY2,XY2S

      ENTRY FR(T)
      TS=T**2
      FRI=EXP(-TS)*(XSYS-TS)/((XSYS-TS)**2+XY2S)
      RETURN

      ENTRY FI(T)
      TS=T**2
      FRI=-EXP(-TS)*XY2/((XSYS-TS)**2+XY2S)
      RETURN

      ENTRY FRX(T)
      TS=T**2
c     IF(ABS(TS-XS) .LT. 170D0)THEN
      FRI=EXP(TS-XS)
c     ELSE
c     FRI=0
c     ENDIF
      RETURN
      END
