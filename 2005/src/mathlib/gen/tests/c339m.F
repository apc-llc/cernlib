*
* $Id: c339m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c339m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C339M
C     This program tests the MATHLIB routines DAWSON and DDAWSN (C339)
#include "gen/imp64.inc"
      REAL DAWSON,SX
      EXTERNAL FC339
      COMMON /FORINTC339/ X,X1

      CHARACTER NAME*6

C     Set numerical tolerance for testing the S/D versions
      DIMENSION TOL(2)
C
      LOGICAL LTEST
#include "iorc.inc"
      DATA TOL / 1D-6,1D-12 /
      CALL HEADER('C339',0)
      ERMAX=0D0
      LTEST=.TRUE.
C
C---  Number of functions to test
#if !defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if defined(CERNLIB_DOUBLE)
      NF=1
#endif
C
      DEPS=1D-16
      DO 9 IDS = NF,2
      IF(IDS .EQ. 1) NAME='DDAWSN'
      IF(IDS .EQ. 2) NAME='DAWSON'
      WRITE(LOUT,100) NAME,NAME
      DO 1 IX = -11,100
      X=IX/10D0
      SX=X
#if defined(CERNLIB_DOUBLE)
      X1=MAX(1D0,ABS(X))
#endif
#if !defined(CERNLIB_DOUBLE)
      X1=MAX(1E0,ABS(X))
#endif
      IF(NAME .EQ. 'DDAWSN') R=DDAWSN(X)
      IF(NAME .EQ. 'DAWSON') R=DAWSON(SX)
      H=X1*R
#if defined(CERNLIB_DOUBLE)
      T=DGAUSS(FC339,0D0,X,DEPS)
#endif
#if !defined(CERNLIB_DOUBLE)
      T= GAUSS(FC339,0D0,X,DEPS)
#endif
      E=0
      IF(H .NE. 0)  E=ABS((H-T)/H)
      ERMAX=MAX(ERMAX,E)
      IF(NAME .EQ. 'DDAWSN') THEN
       LTEST= LTEST .AND. ERMAX .LE. TOL(2)
       WRITE(LOUT,'(1X,F10.1,3D25.16,1P,D10.1)') SX,R,H,T,E
      ELSE
       WRITE(LOUT,'(1X,F10.1,3D25.7,1P,D10.1)') SX,R,H,T,E
#if defined(CERNLIB_DOUBLE)
       LTEST= LTEST .AND. ERMAX .LE. TOL(1)
#endif
#if !defined(CERNLIB_DOUBLE)
       LTEST= LTEST .AND. ERMAX .LE. TOL(2)
#endif
      END IF
    1 CONTINUE
      WRITE(LOUT,'('' Largest Error'',1P,D10.1)') ERMAX
    9 CONTINUE
  100 FORMAT('1'/1X,9X,'X',16X,A6,'(X)',2X,'MAX(1,ABS(X))*',A6,
     1       '(X)',21X,'TEST'/)

C     Check if the test was successful
      IRC=ITEST('C339',LTEST)
      CALL PAGEND('C339')
      RETURN
      END
      FUNCTION FC339(T)
#include "gen/def64.inc"
     +  FC339
      COMMON /FORINTC339/ X,X1
#include "gen/def64.inc"
     +  X,X1,T
      FC339=X1*EXP((T-X)*(T+X))
      RETURN
      END
