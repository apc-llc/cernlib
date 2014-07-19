*
* $Id: c327m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c327m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C327M
C     This program tests MATHLIB routines
C     DEBIR4 , DBSIR4, DEBKR4, DBSKR4,
C     EBSIR4 ,  BSIR4, EBSKR4,  and BSKR4 (C327)
#include "gen/imp64.inc"
      REAL BSIR4,BSKR4,EBSIR4,EBSKR4,R,SX
      LOGICAL LTEST
      EXTERNAL DF1,DF2,DF4N
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-11)
      PARAMETER (STSTERR=1D-6 )
      COMMON /FORINTC327/ X,P
#include "iorc.inc"

      DIMENSION XX(20),N(9)
      CHARACTER Z*52

#if defined(CERNLIB_DOUBLE)
      DTEIR4(TM)=(DGAUSS(DF1,0D0,PI,DEPS*H)-SIN(PI*P)*
     1           DGAUSS(DF2,0D0,TM,DEPS*H))/PI
#endif
#if !defined(CERNLIB_DOUBLE)
      DTEIR4(TM)=( GAUSS(DF1,0E0,PI,DEPS*H)-SIN(PI*P)*
     1            GAUSS(DF2,0E0,TM,DEPS*H))/PI
#endif
      DATA XX /0.00001D0,0.0001D0,0.001D0,0.01D0,0.1D0,0.5D0,1D0,2D0,
     1 3D0,4D0,4.99D0,5D0,6D0,7D0,8D0,9D0,10D0,20D0,50D0,100D0/

      DATA N /1,2,3,-1,-2,-3,1,2,3/
      DATA DEPS /1D-13/

      CALL HEADER('C327',0)
      ERRMAX=0D0
      SERRMAX=0E0
      LTEST= .TRUE.
      PI = 3.14159 26535 89793D0
C---  Number of functions to test
#if defined(CERNLIB_DOUBLE)
      KP=1
#endif
#if !defined(CERNLIB_DOUBLE)
      KP=2
#endif
      DO 9 IDS = KP,2
      DO 1 J = 1,9
      NU=N(J)
      P=NU/4D0
      IF(MOD(ABS(NU),2) .EQ. 1 .AND. J .LE. 6) WRITE(LOUT,100)
      IF(MOD(NU,2) .EQ. 1 .AND. J .GE. 7) WRITE(LOUT,101)
      IF(ABS(NU) .EQ. 2) WRITE(LOUT,'(///)')
#if !defined(CERNLIB_APOLLO)
      DO 2 I = 1,19
#endif
#if defined(CERNLIB_APOLLO)
      DO 2 I =11,18
      IF (I .NE. 13 .AND. I .NE. 14)THEN
#endif
      X=XX(I)
      SX=X
      IF(J .LE. 6) THEN
       IF(IDS .EQ. 1) THEN
        H=DEBIR4(X,NU)
        F=DBSIR4(X,NU)
       ELSE
        H=EBSIR4(SX,NU)
        F=BSIR4(SX,NU)
       ENDIF
       IF(X .LT. 1D0) T=EXP(-X)*DSERIE(X,P)
       IF(X .GE. 1D0) T=DTEIR4(DTMAX(X,P))
      ELSE
       IF(IDS .EQ. 1) THEN
        H=DEBKR4(X,NU)
        F=DBSKR4(X,NU)
       ELSE
        H=EBSKR4(SX,NU)
        F=BSKR4(SX,NU)
       ENDIF
#if defined(CERNLIB_DOUBLE)
       T  =DGAUSS(DF4N,0D0,1D0,DEPS*H)
#endif
#if !defined(CERNLIB_DOUBLE)
       T  = GAUSS(DF4N,0E0,1E0,DEPS*H)
#endif
      ENDIF
      WRITE(Z,'(2D26.16)') H,T
      READ(Z,'(2(D22.16,4X))') H1,T1
       IF(IDS .EQ. 1) THEN
      ERRMAX=MAX(ERRMAX,ABS(H1-T1))
      LTEST= LTEST .AND. ERRMAX .LE.  TSTERR
       ELSE
#if defined(CERNLIB_DOUBLE)
      SERRMAX=MAX(SERRMAX,ABS(H1-T1))
      LTEST= LTEST .AND. SERRMAX .LE.  STSTERR
#endif
#if !defined(CERNLIB_DOUBLE)
      SERRMAX=MAX(SERRMAX,ABS(H1-T1))
      LTEST= LTEST .AND. SERRMAX .LE.  TSTERR
#endif
      ENDIF
      IF(IDS .EQ. 1) THEN
       WRITE(LOUT,'(1X,I4,D10.3,3D25.16,1P,D10.1)') NU,X,H,F,T,
     + ABS(H1-T1)
      ELSE
#if defined(CERNLIB_DOUBLE)
       WRITE(LOUT,'(1X,I4,D10.3,3D25.7,1P,D10.1)') NU,X,H,F,T,ABS(H1-T1)
#endif
#if !defined(CERNLIB_DOUBLE)
       WRITE(LOUT,'(1X,I4,D10.3,3D25.16,1P,D10.1)') NU,X,H,F,T,
     + ABS(H1-T1)
#endif
      ENDIF
#if defined(CERNLIB_APOLLO)
      ENDIF
#endif
    2 CONTINUE
    1 CONTINUE

      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
      IF(IDS .EQ. 1) THEN
       R=DBSIR4(1D0,+5)
       R=DBSIR4(1D0,-5)
       R=DBSIR4(0D0,+1)
       R=DBSIR4(0D0,-1)
       R=DBSIR4(0D0,-5)
       R=DBSKR4(1D0,+5)
       R=DBSKR4(1D0,-5)
       R=DBSKR4(0D0,+1)
       R=DBSKR4(0D0,-1)
       R=DEBIR4(1D0,+5)
       R=DEBIR4(1D0,-5)
       R=DEBIR4(0D0,+1)
       R=DEBIR4(0D0,-1)
       R=DEBIR4(0D0,-5)
       R=DEBKR4(1D0,+5)
       R=DEBKR4(1D0,-5)
       R=DEBKR4(0D0,+1)
       R=DEBKR4(0D0,-1)
      ELSE
       R=BSIR4(1.0,+5)
       R=BSIR4(1.0,-5)
       R=BSIR4(0.0,+1)
       R=BSIR4(0.0,-1)
       R=BSIR4(0.0,-5)
       R=BSKR4(1.0,+5)
       R=BSKR4(1.0,-5)
       R=BSKR4(0.0,+1)
       R=BSKR4(0.0,-1)
       R=EBSIR4(1.0,+5)
       R=EBSIR4(1.0,-5)
       R=EBSIR4(0.0,+1)
       R=EBSIR4(0.0,-1)
       R=EBSIR4(0.0,-5)
       R=EBSKR4(1.0,+5)
       R=EBSKR4(1.0,-5)
       R=EBSKR4(0.0,+1)
       R=EBSKR4(0.0,-1)
      ENDIF
    9 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)')SERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif
  100 FORMAT('1',5X,'I NU/4 (X)'//
     1 1X,2X,'NU',9X,'X',13X,'EXP(-X)*I(X)',21X,'I(X)',21X,'TEST',
     2 5X,'Error'/)
  101 FORMAT('1',5X,'K NU/4 (X)'//
     1 1X,2X,'NU',9X,'X',14X,'EXP(X)*K(X)',21X,'K(X)',21X,'TEST',
     2 5X,'Error'/)
      WRITE(LOUT,'(1X)')

C     Check if the test was successful
      IRC=ITEST('C327',LTEST)
      CALL PAGEND('C327')
      RETURN
      END
      FUNCTION DTMAX(X,P)
#include "gen/imp64.inc"
      DIMENSION I1(3),I2(3),I3(3)

      DATA DEPS /1D-22/
      DATA I1 /1,30,200/, I2 /20,100,3000/, I3 /1,10,100/

      DO 1 K = 1,3
      DO 1 I = I1(K),I2(K),I3(K)
      TM=I/10D0
      DTMAX=TM
       IF(EXP(-X*(1+COSH(TM))-P*TM) .LE. DEPS*EXP(-2*X)) RETURN
    1 CONTINUE
      RETURN
      END
      FUNCTION DF(T)
#include "gen/imp64.inc"
      COMMON /FORINTC327/X,P

      ENTRY DF1(T)
      DF=EXP(X*(COS(T)-1))*COS(P*T)
      RETURN

      ENTRY DF2(T)
      DF=EXP(-X*(COSH(T)+1)-P*T)
      RETURN

      ENTRY DF4N(T)
      IF( (P*T/(1-T))  .LT. 30D0 .AND. (T/(1-T)) .LT. 30D0
     +    .AND. ABS(X*(1-COSH(T/(1-T)))) .LE. 50) THEN
      DF=(1/(1-T)**2)*EXP( X*(1-COSH(T/(1-T))))
     +*COSH(P*T/(1-T))
      ELSE
      DF=0
      ENDIF
      RETURN

      END
      FUNCTION DSERIE(X,P)
#include "gen/imp64.inc"

      Y=(X/2)**2
#if defined(CERNLIB_DOUBLE)
      A=1/DGAMMA(P+1)
#endif
#if !defined(CERNLIB_DOUBLE)
      A=1/ GAMMA(P+1)
#endif
      S=A
      K=0
    1 K=K+1
      A=(1/(K*(P+K)))*A*Y
      S=S+A
      IF(ABS(A) .GT. 1D-20) GO TO 1
      DSERIE=(X/2)**P*S
      RETURN
      END
