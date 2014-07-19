*
* $Id: c317m.F,v 1.1.1.1 1996/04/01 15:01:15 mclareni Exp $
*
* $Log: c317m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C317M
#include "iorc.inc"
C     This program tests the GENLIB routines ADIGAM and DDIGAM (C317)
C     by comparing computed results against some known theoretical
C     results.
C     Written by T Hepworth, Brunel University, England, 20.4.90
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DDIGAM,X(7),EXACT(7),RESULT(7),ERROR(7),ERRMAX
      DOUBLE PRECISION TSTERR
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL   ADIGAM,X(7),EXACT(7),RESULT(7),ERROR(7),ERRMAX,TSTERR
#endif
C     Set the maximum error allowed for the test to still be considered
C     successful
      PARAMETER ( TSTERR=1D-13)

C     Set up test data and theoretical solutions
      DATA X(1),EXACT(1) /  0.4D0, -0.2561384544585116D+01 /
      DATA X(2),EXACT(2) /  3.6D0,  0.1135662837388860D+01 /
      DATA X(3),EXACT(3) /  6.0D0,  0.1706117668431800D+01 /
      DATA X(4),EXACT(4) / 13.6D0,  0.2572854781961887D+01 /
      DATA X(5),EXACT(5) / 14.0D0,  0.2602918090232222D+01 /
      DATA X(6),EXACT(6) / 14.4D0,  0.2632104300151583D+01 /
      DATA X(7),EXACT(7) / 20.0D0,  0.2970523992242149D+01 /

      CALL HEADER('C317',0)
C     Initialise maximum error
      ERRMAX=0D0
      WRITE(LOUT,'(5X,''X'',8X,''Calculated Value'',13X,
     +        ''Exact Value'',10x,''Absolute Error'')')
      DO 100 I=1,7
#if defined(CERNLIB_DOUBLE)
         RESULT(I)=DDIGAM(X(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         RESULT(I)=ADIGAM(X(I))
#endif
C        Calculate absolute errors
         ERROR(I)= ABS ( RESULT(I)-EXACT(I) )
         ERRMAX=MAX ( ERRMAX,ERROR(I) )
         WRITE(LOUT,'(F6.1,3F24.16)') X(I),RESULT(I),EXACT(I),ERROR(I)
100   CONTINUE
      WRITE(LOUT,'('' Largest Error'',F24.16)') ERRMAX

C     Check if the test was successful
      IRC=ITEST('C317',ERRMAX .LE. TSTERR)
      CALL PAGEND('C317')
      RETURN
      END
