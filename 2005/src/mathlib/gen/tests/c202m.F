*
* $Id: c202m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c202m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C202M
C     ROUTINE TO TEST MATHLIB ROUTINES RMULLZ AND DMULLZ (C202)
#include "gen/imp64.inc"
C     Set the total number of tests and the maximum degree of the
C     polynomials to be tested
      PARAMETER ( NT=6)
      PARAMETER ( NA=5)
      CHARACTER*6 TFUNC(2)
C
C     Set numerical tolerance for testing the S/D versions
      DIMENSION TOL(1)
C
#include "gen/defc64.inc"
     +     Z0(NA),SOL(1:NT,1:NA),EXACT(1:NT,1:NA)
      DIMENSION A(0:NA,0:NT),ND(0:NT),ERROR(1:NT,1:NA),AA(0:4,1)
      LOGICAL LTEST
#include "iorc.inc"
      DATA TFUNC/'RMULLZ','DMULLZ'/
      DATA TOL / 1D-11 /
C     Input parameters for individual tests
       DATA ND /2,1,2,3,4,2,3/
       DATA (A(J,1),J=0,1)/1,-1/
       DATA (A(J,2),J=0,2)/1,-3,2/
       DATA (A(J,3),J=0,3)/1,-6,11,-6/
       DATA (A(J,4),J=0,4)/1,9,-79,159,-90/
       DATA (A(J,5),J=0,2)/1,-20,500/
       DATA (A(J,6),J=0,3)/1,-453.00000000,68402.00000000,
     +                     -3442800.00000000/
C     Analytical values expected to be obtained
        DATA EXACT(1,1) / (1,0)/
        DATA (EXACT(2,J),J=1,2) / (1,0),(2,0)/
        DATA (EXACT(3,J),J=1,3) / (1,0),(3,0),(2,0)/
        DATA (EXACT(4,J),J=1,4) / (-15,0),(3,0),(1,0),(2,0)/
        DATA (EXACT(5,J),J=1,2) / (10,-20),(10,20)/
        DATA (EXACT(6,J),J=1,3) / (150,0),(152,0),(151,0)/
        DATA (AA(J,1),J=0,4)/1,-5,6,4,-8/

      CALL HEADER('C202',0)
      ERRMAX=0D0
      MAXITR=100000
      LTEST=.TRUE.
C
        ERRMAX=0D0
#if defined(CERNLIB_DOUBLE)
      DO 100 I=1,NT
#endif
#if !defined(CERNLIB_DOUBLE)
      DO 100 I=1,5
        WRITE(LOUT,'(/8X,'' '',A,''(X)'',23X,''Exact Value'',10X,
     +  ''Rel Error'')')   TFUNC(1)
         CALL  RMULLZ(A(0,I),ND(I),MAXITR,Z0)
      DO 22 J=1,ND(I)
         SOL(I,J)=Z0(J)
         IF (Z0(J) .NE. 0)
     +   ERROR(I,J)= ABS(  ABS(EXACT(I,J))-  ABS(Z0(J)))/  ABS(Z0(J))
         ERRMAX= MAX( ERRMAX,ERROR(I,J) )
 22        WRITE(LOUT,444) SOL(I,J),EXACT(I,J),ERROR(I,J)
#endif
#if defined(CERNLIB_DOUBLE)
        WRITE(LOUT,'(/13X,'' '',A,''(X)'',23X,''Exact Value'',10X,
     +  ''Rel Error'')')   TFUNC(2)
         CALL  DMULLZ(A(0,I),ND(I),MAXITR,Z0)
      DO 44 J=1,ND(I)
         SOL(I,J)=Z0(J)
         IF (Z0(J) .NE. 0)
     +   ERROR(I,J)= ABS(  ABS(EXACT(I,J))-  ABS(Z0(J)))/  ABS(Z0(J))
         ERRMAX= MAX( ERRMAX,ERROR(I,J) )
 44        WRITE(LOUT,444) SOL(I,J),EXACT(I,J),ERROR(I,J)
#endif
  444 FORMAT (1X,'(',F14.10,F14.10,')',5X,'(',F14.10,F14.10,')',D10.1)
100   CONTINUE
        WRITE(LOUT,'(/''Largest Relative Error was'',D10.1)')ERRMAX
        LTEST=LTEST.AND.(ERRMAX.LE. TOL(1))

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      MAXITR=10
#if defined(CERNLIB_DOUBLE)
      CALL DMULLZ(AA(0,1),ND(4),MAXITR,Z0)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RMULLZ(AA(0,1),ND(4),MAXITR,Z0)
#endif
      DO 3 J = 1,ND(4)
  3        WRITE(LOUT,555) Z0(J)
  555 FORMAT (1X,'(',D10.1,D10.1,')')
      A(0,0)=0
#if defined(CERNLIB_DOUBLE)
      CALL DMULLZ(A(0,0),ND(0),MAXITR,Z0)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RMULLZ(A(0,0),ND(0),MAXITR,Z0)
#endif

1000  CONTINUE
      IRC=ITEST('C202',LTEST)
      CALL PAGEND('C202')
      RETURN
      END
