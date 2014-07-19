*
* $Id: b102m.F,v 1.1.1.1 1996/04/01 15:01:12 mclareni Exp $
*
* $Log: b102m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:12  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE B102M
C     Routine to test MATHLIB routines ASINH and DASINH (B102)
#include "gen/imp64.inc"
C     Set the total number of tests
      PARAMETER (NT=4)
C
      CHARACTER*6 TFUNC(2)
C
C     Set numerical tolerance for testing the S/D versions
      DIMENSION TOL(2)
C
      DIMENSION X(NT),EXACT(NT)
      REAL ASINH
      LOGICAL LTEST
#include "iorc.inc"
C     Input parameters for individual tests
C     Index of ERROR, 1=Real test error

      DATA X( 1 )/ -1.000D0/, EXACT( 1 ) /-8.813735870195429920D-01/
      DATA X( 2 )/  0.000D0/, EXACT( 2 ) /0.000000000000000000D+00 /
      DATA X( 3 )/  1.000D0/, EXACT( 3 ) /8.813735870195429920D-01 /
      DATA X( 4 )/ -0.014D0/, EXACT( 4 ) /-1.399954270699875840D-02/

      DATA TFUNC/'ASINH','DASINH'/
      DATA TOL / 1D-6,1D-14 /

      CALL HEADER('B102',0)
      LTEST=.TRUE.
C
C---  Number of functions to test
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
C
      DO 200 JF=1,NF
        WRITE(LOUT,'(/8X,''X'',14X,'' '',A,''(X)'',7X,''Rel Error'')')
     +      TFUNC(JF)
        ERRMAX=0D0
c       ERROR =1D3
      DO 100 I=1,NT
#if !defined(CERNLIB_DOUBLE)
                      SOL  = ASINH( X(I) )
         IF(SOL.NE.0) ERROR= ABS( (EXACT(I)-SOL)/SOL )
         WRITE(LOUT,'(1X,F10.3,1P,D27.18,D10.1)') X(I),SOL,ERROR
#endif
#if defined(CERNLIB_DOUBLE)
         IF(JF.EQ.1) THEN
                      SOL  = ASINH(SNGL( X(I)) )
         IF(SOL.NE.0) ERROR= ABS( SNGL(EXACT(I)-SOL)/SNGL(SOL) )
         WRITE(LOUT,'(1X,F10.3,1P,E27.9,E10.1)')
     +         SNGL(X(I)),SNGL(SOL),SNGL(ERROR)
         ENDIF
         IF(JF.EQ.2) THEN
                      SOL  =DASINH( X(I) )
         IF(SOL.NE.0) ERROR= ABS( (EXACT(I)-SOL)/SOL )
         WRITE(LOUT,'(1X,F10.3,1P,D27.18,D10.1)') X(I),SOL,ERROR
         ENDIF
#endif
         ERRMAX= MAX( ERRMAX,ERROR )
100   CONTINUE
        WRITE(LOUT,'(/''Largest Relative Error was'',1P,D10.1)')ERRMAX
#if !defined(CERNLIB_DOUBLE)
         ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
         ETOL=TOL(JF  )
#endif
        LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
200   CONTINUE

      IRC=ITEST('B102',LTEST)
      CALL PAGEND('B102')
      RETURN
      END
