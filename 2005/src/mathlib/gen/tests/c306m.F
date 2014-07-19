*
* $Id: c306m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c306m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C306M
#include "iorc.inc"
C     This program tests the operation of MATHLIB routines
C     CGAMMA and WGAMA(C306)
C     Set the number of tests
      PARAMETER ( NT=6)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION   ERROR(NT),ERRMAX
      COMPLEX*16         Z(NT),WGAMMA,EXACT(NT),SOL(NT)
      COMPLEX                  CGAMMA
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL               ERROR(NT),ERRMAX
      COMPLEX            Z(NT),CGAMMA,EXACT(NT),SOL(NT)
#endif
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-13)
C     Set up the test parameters EXACT holds analytical solution
      DATA (Z(J),J=1,3) / (1,-2.5),(1,2),(0,6.5) /
      DATA (Z(J),J=4,6) / (0.25,-4),(-0.25,-4),(-3.25,0) /
#if defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) /( 0.6687277236487296D-01,-0.4032263511948419D-01)/
      DATA EXACT(2) /( 0.1519040026700361D+00, 0.1980488016185498D-01)/
      DATA EXACT(3) /( 0.5623900528200945D-05,-0.3573530195500522D-04)/
      DATA EXACT(4) /( 0.1337354916562139D-02,-0.3029538475189193D-02)/
      DATA EXACT(5) /( 0.1573616266497059D-02,-0.4993363531906417D-03)/
      DATA EXACT(6) /( 0.5362507279163853D+00, 0.0000000000000000D+00)/
#endif
#if !defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) /( 0.6687277236487296E-01,-0.4032263511948419E-01)/
      DATA EXACT(2) /( 0.1519040026700361, 0.1980488016185498E-01)/
      DATA EXACT(3) /( 0.5623900528200945E-05,-0.3573530195500522E-04)/
      DATA EXACT(4) /( 0.1337354916562139E-02,-0.3029538475189193E-02)/
      DATA EXACT(5) /( 0.1573616266497059E-02,-0.4993363531906417E-03)/
      DATA EXACT(6) /( 0.5362507279163853, 0)/
#endif

      CALL HEADER('C306',0)

C     Compare the computed results with exact results for a selection
C     of values
      ERRMAX=0D0
      DO 100 I=1,NT
         WRITE(LOUT,'('' Test number'',I3)') I
#if defined(CERNLIB_DOUBLE)
         SOL(I)=WGAMMA(Z(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         SOL(I)=CGAMMA(Z(I))
#endif
         ERROR(I)=ABS( SOL(I)-EXACT(I) )
         WRITE(LOUT,'('' Calculated   '',2F25.16)') SOL(I)
         WRITE(LOUT,'('' Exact        '',2F25.16)') EXACT(I)
         WRITE(LOUT,'('' Abs error    '',F25.16)') ERROR(I)
         ERRMAX=MAX( ERRMAX,ERROR(I) )
 100  CONTINUE
      WRITE(LOUT,'(/'' Largest Error'',F25.16)') ERRMAX

      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
       R=WGAMMA((0D0,0D0))
       R=WGAMMA((-3D0,0D0))
#endif
       R=CGAMMA((0.,0.))
       R=CGAMMA((-3.,0.))
C     Check if the test was successful
      IRC=ITEST('C306',ERRMAX .LE. TSTERR)
      CALL PAGEND('C306')
      RETURN
      END
