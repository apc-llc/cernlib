*
* $Id: d401m.F,v 1.1.1.1 1996/04/01 15:01:24 mclareni Exp $
*
* $Log: d401m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:24  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D401M
C     This program tests the MATHLIB routines DERIV,DDERIV (D401)

#include "gen/imp64.inc"
      EXTERNAL FD401
      COMMON /LABEL/ L
      DIMENSION X(20),D(20)
      CHARACTER*10 H(20)
      PARAMETER ( TSTERR=5D-10)

#include "iorc.inc"

      DATA X( 1),D( 1),H( 1) /0.04D0 ,1   ,'1/TAN(X)  '/
      DATA X( 2),D( 2),H( 2) /0.001D0,1   ,'SIN(1/X)  '/
      DATA X( 3),D( 3),H( 3) /0.5D0  ,1   ,'X**2      '/
      DATA X( 4),D( 4),H( 4) /0.999D0,1   ,'10**8*X**4'/
      DATA X( 5),D( 5),H( 5) /1      ,1   ,'1         '/
      DATA X( 6),D( 6),H( 6) /0.002D0,1   ,'X*SIN(1/X)'/
      DATA X( 7),D( 7),H( 7) /1.001D0,1   ,'X**99     '/
      DATA X( 8),D( 8),H( 8) /100    ,50  ,'ATAN(X)   '/
      DATA X( 9),D( 9),H( 9) /2      ,1   ,'X**X      '/
      DATA X(10),D(10),H(10) /1D-5   ,1D-4,'SQRT(X)   '/
      DATA X(11),D(11),H(11) /0.001D0,1   ,'SIN(1/X)/X'/
      DATA X(12),D(12),H(12) /0.5D0  ,1   ,'RANF(X)   '/
      DATA X(13),D(13),H(13) /0      ,1   ,'ABS(X)    '/
      DATA X(14),D(14),H(14) /0      ,1   ,'1/X       '/

      CALL HEADER('D401',0)

      ERRMAX=0D0
      WRITE(LOUT,100)
      DO 1 L = 1,14
      IF(L .EQ. 12) THEN
      WRITE(LOUT,'(/'' Largest error'',1P,D10.1)') ERRMAX
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      ENDIF
      IF(L .EQ. 13)
     +WRITE(LOUT,'(/''TESTING NON DIFFERENTIABLE FUNCTIONS:''/)')
      DEL=D(L)
#if defined(CERNLIB_DOUBLE)
      CALL DDERIV(FD401,X(L),DEL,DFDX,RERR)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  DERIV(FD401,X(L),DEL,DFDX,RERR)
#endif
      TST=TD401(X(L))
      IF(L .LT. 12)THEN
      IF(TST .NE. 0)ERRMAX=MAX(ERRMAX,ABS((DFDX-TST)/TST))
      IF(TST .EQ. 0)ERRMAX=MAX(ERRMAX,ABS(DFDX-TST))
      ENDIF
      WRITE(LOUT,'(1X,A10,3F12.8,2F30.15,1PD10.1/)')
     1            H(L),X(L),D(L),DEL,DFDX,TST,RERR
    1 CONTINUE
  100 FORMAT('1'/1X,10X,11X,'X',7X,'DELTA',6X,'DELTA''',
     1       25X,'DF/DX',26X,'TEST',6X,'RERR'/)
C     Check if the test was successful
      IRC=ITEST('D401',ERRMAX .LE. TSTERR)
      CALL PAGEND('D401')
      RETURN
      END
      FUNCTION FD401(X)
#include "gen/imp64.inc"
      COMMON /LABEL/ L

      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14), L

    1 FD401=1/TAN(X)
      RETURN
    2 FD401=SIN(1/X)
      RETURN
    3 FD401=X**2
      RETURN
    4 FD401=1D8*X**4
      RETURN
    5 FD401=1
      RETURN
    6 FD401=X*SIN(1/X)
      RETURN
    7 FD401=X**99
      RETURN
    8 FD401=ATAN(X)
      RETURN
    9 FD401=X**X
      RETURN
   10 FD401=SQRT(X)
      RETURN
   11 FD401=SIN(1/X)/X
      RETURN
   12 FD401=RANF()
      RETURN
   13 FD401=ABS(X)
      RETURN
   14 FD401=1/X
      RETURN
      END
      FUNCTION TD401(X)
#include "gen/imp64.inc"
      COMMON /LABEL/ L

      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14), L

    1 TD401=-1/SIN(X)**2
      RETURN
    2 TD401=-COS(1/X)/X**2
      RETURN
    3 TD401=2*X
      RETURN
    4 TD401=4D8*X**3
      RETURN
    5 TD401=0
      RETURN
    6 TD401=SIN(1/X)-COS(1/X)/X
      RETURN
    7 TD401=99*X**98
      RETURN
    8 TD401=1/(1+X**2)
      RETURN
    9 TD401=X**X*(1+LOG(X))
      RETURN
   10 TD401=1/(2*SQRT(X))
      RETURN
   11 TD401=-(SIN(1/X)+COS(1/X)/X)/X**2
      RETURN
   12 TD401=99999D0
      RETURN
   13 TD401=99999D0
      RETURN
   14 TD401=99999D0
      RETURN
      END
