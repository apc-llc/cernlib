*
* $Id: d113m.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d113m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D113M
#include "iorc.inc"
C     Program to test the MATHLIB routines CGAUSS,WGAUSS (D113) by
C     numerical evaluation of complex line integrals, which can be
C     evaluated analytically.

      EXTERNAL D113F1,D113F2,D113F3
      COMMON /D113I1/ A,B
      COMMON /D113I3/ NC
#include "gen/defc64.inc"
     +    WGAUSS,D113F1,D113F2,D113F3,A,B,
     +    ATAB,BTAB,G1,G2,G3,D113F,G
#include "gen/def64.inc"
     +    EPSTAB,EPS,ERRMAX,ABS
#if !defined(CERNLIB_DOUBLE)
#include "gen/defc64.inc"
     +    CGAUSS
#endif
      DIMENSION ATAB(20),BTAB(20),EPSTAB(20)

C     Set the largest absolute error allowed for a successful test
      PARAMETER ( TSTERR=5D-07)
#if defined(CERNLIB_DOUBLE)
      DATA ATAB(1) /(-1D0, 0D0)/, BTAB(1) /( 2D0,0D0)/
      DATA ATAB(2) /( 0D0,-1D0)/, BTAB(2) /( 0D0,2D0)/
      DATA ATAB(3) /( 0D0, 2D0)/, BTAB(3) /( 2D0,1D0)/
      DATA (EPSTAB(I),I=1,4) /1D-2,1D-5,1D-7,1D-12/
#endif
#if !defined(CERNLIB_DOUBLE)
      DATA ATAB(1) /(-1E0, 0E0)/, BTAB(1) /( 2E0,0E0)/
      DATA ATAB(2) /( 0E0,-1E0)/, BTAB(2) /( 0E0,2E0)/
      DATA ATAB(3) /( 0E0, 2E0)/, BTAB(3) /( 2E0,1E0)/
      DATA (EPSTAB(I),I=1,4) /1D-2,1D-5,1D-6,1D-12/
#endif

      G1(A,B)=EXP(B)-EXP(A)
      G2(A,B)=2*(B*SQRT(B)-A*SQRT(A))/3
      G3(A,B)=2*(SQRT(B-A)-SQRT(A-B))

      CALL HEADER('D113',0)
      ERRMAX=0D0

#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''JFN'',2X,''EPS'',10X,''A'',11X,''B'',7X,''NC'',
     +17X, ''WGAUSS'',20X,''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''JFN'',2X,''EPS'',10X,''A'',11X,''B'',7X,''NC'',
     +17X, ''CGAUSS'',20X,''Error'')')
#endif
      DO 1 JFN = 1,3
      IF(JFN .GT. 1) WRITE(LOUT,'(1X)')
      DO 1 JAB = 1,3
      WRITE(LOUT,'(1X)')
      A=ATAB(JAB)
      B=BTAB(JAB)
      DO 1 JEPS = 1,4
      IF(.NOT. (JFN .EQ. 3 .AND. JEPS .EQ. 4)) THEN
      EPS=EPSTAB(JEPS)
      NC=0
      IF(JFN .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
       D113F=WGAUSS(D113F1,A,B,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
       D113F=CGAUSS(D113F1,A,B,EPS)
#endif
       G=G1(A,B)
      ELSE IF(JFN .EQ. 2) THEN
#if defined(CERNLIB_DOUBLE)
       D113F=WGAUSS(D113F2,A,B,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
       D113F=CGAUSS(D113F2,A,B,EPS)
#endif
       G=G2(A,B)
      ELSE
#if defined(CERNLIB_DOUBLE)
       D113F=WGAUSS(D113F3,A,B,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
       D113F=CGAUSS(D113F3,A,B,EPS)
#endif
       G=G3(A,B)
      END IF
      IF(JEPS .EQ. 3)  ERRMAX=MAX(ERRMAX,ABS((D113F-G)/G))
      WRITE(LOUT,101) JFN,EPS,A,B,NC,D113F,ABS((D113F-G)/G)
      END IF
    1 CONTINUE
  101 FORMAT(1X,I2,D8.1,2(F6.1,F6.1),I5,2F20.16,1P,D10.1)
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      D113F=WGAUSS(D113F3,ATAB(3),BTAB(3),EPSTAB(4))
#endif
#if !defined(CERNLIB_DOUBLE)
      D113F=CGAUSS(D113F3,ATAB(3),BTAB(3),EPSTAB(4))
#endif
      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('D113',ERRMAX .LE. TSTERR)
      CALL PAGEND('D113')
      RETURN
      END

      FUNCTION D113FF(Z)
#include "gen/impc64.inc"
      COMMON /D113I1/ A,B
      COMMON /D113I3/ NC

      ENTRY D113F1(Z)
      NC=NC+1
      D113F1=EXP(Z)
      RETURN

      ENTRY D113F2(Z)
      NC=NC+1
      D113F2=SQRT(Z)
      RETURN

      ENTRY D113F3(Z)
      NC=NC+1
      D113F3=1/SQRT(Z-A)+1/SQRT(Z-B)
      RETURN
      END
