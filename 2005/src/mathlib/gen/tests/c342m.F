*
* $Id: c342m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c342m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C342M
C     This program tests the MATHLIB routines STRH0,DSTRH0,STRH1,DSTRH1,
C     (C342)

#include "gen/imp64.inc"
#if defined(CERNLIB_DOUBLE)
      REAL STRH0,STRH1,SX
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL             SX
#endif
      EXTERNAL C342F0,C342F1
      COMMON /FORINTC342/ X
      CHARACTER NAME*6,Z*52
      DIMENSION TOL(2)

      LOGICAL LTEST

#include "iorc.inc"
      DATA TOL/5D-5, 8D-13/
      DATA LTEST/.TRUE./
      CALL HEADER('C342',0)

#if defined(CERNLIB_DOUBLE)
      NF=4
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=2
#endif
      DEPS=1.0D-16

      PI = 3.14159 26535 89793D0
      DO 9 IIX = 1,2
      DO 9 IDS = 1,NF
      ERMAX=0
      IF(IDS .EQ. 1) NAME='STRH0 '
      IF(IDS .EQ. 2) NAME='STRH1 '
      IF(IDS .EQ. 3) NAME='DSTRH0'
      IF(IDS .EQ. 4) NAME='DSTRH1'
      WRITE(LOUT,100) NAME
      DO 1 IX = -20,140,2
      X=IX/10D0**IIX
      SX=X
      IF(IDS .EQ. 1) R=STRH0(SX)
      IF(IDS .EQ. 2) R=STRH1(SX)
      IF(IDS .EQ. 3) R=DSTRH0(X)
      IF(IDS .EQ. 4) R=DSTRH1(X)
      IF(IDS .EQ. 3 .OR. IDS .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
       T=(2/PI)*DGAUSS(C342F0,0D0,PI/2,DEPS*ABS(R))
#endif
#if !defined(CERNLIB_DOUBLE)
       T=(2/PI)*GAUSS(C342F0,0D0,PI/2,DEPS*ABS(R))
#endif
      ELSE
#if defined(CERNLIB_DOUBLE)
       T=(2/PI)*X*DGAUSS(C342F1,0D0,PI/2,DEPS*ABS(R))
#endif
#if !defined(CERNLIB_DOUBLE)
       T=(2/PI)*X* GAUSS(C342F1,0D0,PI/2,DEPS*ABS(R))
#endif
      END IF
      WRITE(Z,'(2D26.16)') R,T
      READ(Z,'(2(D22.16,4X))') R1,T1
      ERMAX= MAX(ERMAX,ABS(R1-T1))
      IF(IDS .EQ. 3 .OR. IDS .EQ. 4) THEN
       WRITE(LOUT,'(1X,F10.3,2F25.16,1P,D10.1)') SX,R,T,ABS(R1-T1)
      ELSE
       WRITE(LOUT,'(1X,F10.3,2F25.7,1P,D10.1)') SX,R,T,ABS(R1-T1)
      END IF
#if defined(CERNLIB_DOUBLE)
       LTEST= LTEST .AND. ERMAX .LE. TOL(1)
#endif
#if !defined(CERNLIB_DOUBLE)
       LTEST= LTEST .AND. ERMAX .LE. TOL(2)
#endif
    1 CONTINUE
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERMAX
    9 CONTINUE
  100 FORMAT(/1X,9X,'X',16X,A6,'(X)',18X,'TEST',6X,'ERROR'/)
C     Check if the test was successful
      IRC=ITEST('C342',LTEST)
      CALL PAGEND('C342')
      RETURN
      END
      FUNCTION C342F(T)
#include "gen/imp64.inc"
      COMMON /FORINTC342/ X

      ENTRY C342F0(T)
      C342F=SIN(X*COS(T))
      RETURN

      ENTRY C342F1(T)
      C342F=SIN(X*COS(T))*SIN(T)**2
      RETURN
      END
