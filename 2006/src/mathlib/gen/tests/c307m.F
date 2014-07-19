*
* $Id: c307m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c307m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C307M
#include "iorc.inc"
C     This program tests the operation of MATHLIB routines
C     CDIGAM and WDIGAM(C307)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION   ERROR(7),ERRMAX
      COMPLEX*16         Z(7),WDIGAM,EXACT(7),SOL(7)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL               ERROR(7),ERRMAX
      COMPLEX            Z(7),CDIGAM,EXACT(7),SOL(7)
#endif
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-13 )
C     Set up the test parameters EXACT holds analytical solution
      DATA (Z(J),J=1,3) /(-20.5,1),(-15,-2.5),(-7.5,4)/
      DATA (Z(J),J=4,7)/(0,-1),(7.5,0),(15,2),(20.5,0)/
#if defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) / (0.3045748746354412D+01, 0.3082306882944267D+01)/
      DATA EXACT(2) / (0.2753841362080107D+01,-0.2981733550186298D+01)/
      DATA EXACT(3) / (0.2191326129404467D+01, 0.2678360620124173D+01)/
      DATA EXACT(4) / (0.9465032062247780D-01,-0.2076674047468579D+01)/
      DATA EXACT(5) / (0.1946757484246086D+01, 0.0000000000000000D+00)/
      DATA EXACT(6) / (0.2683758858614923D+01, 0.1370136096416038D+00)/
      DATA EXACT(7) / (0.2995836394707646D+01, 0.0000000000000000D+00)/
#endif
#if !defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) / (0.3045748746354412E+01, 0.3082306882944267E+01)/
      DATA EXACT(2) / (0.2753841362080107E+01,-0.2981733550186298E+01)/
      DATA EXACT(3) / (0.2191326129404467E+01, 0.2678360620124173E+01)/
      DATA EXACT(4) / (0.9465032062247780E-01,-0.2076674047468579E+01)/
      DATA EXACT(5) / (0.1946757484246086E+01, 0)/
      DATA EXACT(6) / (0.2683758858614923E+01, 0.1370136096416038)/
      DATA EXACT(7) / (0.2995836394707646E+01, 0)/
#endif
      CALL HEADER('C307',0)

C     Compare the computed results with exact results for a selection
C     of values
      ERRMAX=0D0
      DO 100 I=1,7
         WRITE(LOUT,'('' Test number'',I3)') I
#if defined(CERNLIB_DOUBLE)
         SOL(I)=WDIGAM(Z(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         SOL(I)=CDIGAM(Z(I))
#endif

         ERROR(I)=ABS( SOL(I)-EXACT(I) )
         WRITE(LOUT,'('' Calculated   '',2F25.16)') SOL(I)
         WRITE(LOUT,'('' Exact        '',2F25.16)') EXACT(I)
         WRITE(LOUT,'('' Abs error    '',F25.16)') ERROR(I)
         ERRMAX=MAX( ERRMAX,ERROR(I) )
 100  CONTINUE
      WRITE(LOUT,'('' Largest Error'',F25.16)') ERRMAX

C     Check if the test was successful
      IRC=ITEST('C307',ERRMAX .LE. TSTERR)
      CALL PAGEND('C307')
      RETURN
      END
