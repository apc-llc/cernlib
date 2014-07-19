*
* $Id: g106m.F,v 1.1.1.1 1996/04/01 15:01:29 mclareni Exp $
*
* $Log: g106m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:29  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G106M
C     This program tests the GENLIB routine GAMDIS (G106)

      EXTERNAL FG106
      PARAMETER ( TSTERR=5E-5 )
      COMMON /AAG106/ A
#include "iorc.inc"

      CALL HEADER('G106',0)
C     Initialise maximum error
      ERRMAX=0.0
      WRITE(LOUT,'(/6X,''X'',6X,''A'',5X,''Calculated'',7X,
     +        ''Test'',10X,''Error'')')
      DO 1 IX = 0,100,2
      X=0.1*IX
      IF(IX .EQ. 0) X=0.01
      IF(IX .NE. 0)WRITE(LOUT,'(//)')
      DO 1 IA = 0,500,50
      A=0.01*IA
      IF(IA .EQ. 0) A=0.01
      P=GAMDIS(X,A)
      X0=MIN(X,0.1)
      TP=X0**A/A
      U=TP
      DO 9 K = 0,6
      U=-U*X0*(K+A)/((K+1+A)*(K+1))
    9 TP=TP+U
      TP=(TP+GAUSS(FG106,X0,X,P*5E-6))/GAMMA(A)
      IF(P .NE. 0)ERR=ABS((P-TP)/P)
      IF(P .EQ. 0)ERR=ABS(P-TP)
      ERRMAX=MAX(ERRMAX,ERR)
      WRITE(LOUT,'(1X,2F7.2,2E15.6,1P,D10.1)') X,A,P,TP,ERR
    1 CONTINUE
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
      P=GAMDIS(-1.,0.)
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX

C     Check if the test was successful
      IRC=ITEST('G106',ERRMAX .LE. TSTERR)
      CALL PAGEND('G106')
      RETURN
      END
      FUNCTION FG106(T)
      COMMON /AAG106/ A

      FG106=EXP(-T)*T**(A-1)
      RETURN
      END
