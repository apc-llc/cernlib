*
* $Id: c328m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c328m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C328M
C     Test the MATHLIB routines CWHITM and WWHITM (C328)
C     Set the number of tests
      PARAMETER ( NT=4 )
#include "gen/def64.inc"
     +      ERROR(NT),ERRMAX
#if defined(CERNLIB_DOUBLE)
#include "gen/defc64.inc"
     +      WWHITM
      REAL  SERROR(NT),SERRMAX
      COMPLEX SZ(NT),SKA(NT),SMU(NT),SEXACT(NT),SSOL(NT)
#endif
#include "gen/defc64.inc"
     +      Z(NT),KA(NT),MU(NT),EXACT(NT),SOL(NT)
      COMPLEX CWHITM
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-12)
#if defined(CERNLIB_DOUBLE)
      PARAMETER (STSTERR=1D-6 )
#endif
      LOGICAL LTEST
#include "iorc.inc"
C     Set up the test parameters EXACT holds analytical solution
      DATA KA(1),MU(1),Z(1) / (1,0),         (2,0),   (5,-3) /
#if !defined(CERNLIB_CMPXDOUB)
      DATA KA(2),MU(2),Z(2)/(-2.25E0,0E0),(-0.25E0,0E0), (0E0,2E0) /
#endif
#if defined(CERNLIB_CMPXDOUB)
      DATA KA(2),MU(2),Z(2) / (-2.25D0,0D0), (-0.25D0,0D0), (0D0,2D0) /
      DATA SKA(2),SMU(2),SZ(2)/(-2.25E0,0E0),(-0.25E0,0E0), (0E0,2E0) /
#endif
      DATA KA(3),MU(3),Z(3) / (3,5), (1,1), (-2,6) /
      DATA KA(4),MU(4),Z(4) / (3,5), (1,1), (-2,4) /
#if defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) /( 0.768837780746D+01, -0.402275799430D+02) /
      DATA EXACT(2) /(-0.102761073680D+02, -0.338630674302D+01) /
      DATA EXACT(3) /( 0.133052729239D+03,  0.713294437705D+03) /
      DATA EXACT(4) /(-0.896279277944D+02,  0.848021596620D+02) /
#endif
#if !defined(CERNLIB_CMPXDOUB)
      DATA EXACT(1) /( 0.768837780746E+01, -0.402275799430E+02) /
      DATA EXACT(2) /(-0.102761073680E+02, -0.338630674302E+01) /
      DATA EXACT(3) /( 0.133052729239E+03,  0.713294437705E+03) /
      DATA EXACT(4) /(-0.896279277944E+02,  0.848021596620E+02) /
#endif

      CALL HEADER('C328',0)

C     Compare the computed results with exact results for a selection
C     of values
      ERRMAX=0D0
#if defined(CERNLIB_DOUBLE)
      SERRMAX=0E0
#endif
      LTEST=.TRUE.
      DO 100 I=1,NT
#if defined(CERNLIB_DOUBLE)
      SEXACT(I)=EXACT(I)
      IF (I .NE.2) THEN
      SZ(I)=Z(I)
      SKA(I)=KA(I)
      SMU(I)=MU(I)
      ENDIF
#endif
         WRITE(LOUT,'(/'' Test number'',I3/)') I
#if defined(CERNLIB_DOUBLE)
         SOL(I)=WWHITM(Z(I),KA(I),MU(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         SOL(I)=CWHITM(Z(I),KA(I),MU(I))
#endif
         ERROR(I)=ABS( (SOL(I)-EXACT(I))/SOL(I) )
         WRITE(LOUT,'('' Calculated   '',2F25.16)') SOL(I)
         WRITE(LOUT,'('' Exact        '',2F25.16)') EXACT(I)
         WRITE(LOUT,'('' Rel error    '',4X,1P,D10.1)') ERROR(I)
         ERRMAX=MAX( ERRMAX,ERROR(I) )
         LTEST= LTEST .AND. ERRMAX .LE. TSTERR
#if defined(CERNLIB_DOUBLE)
         WRITE(LOUT,'(/'' Test of Single Precision Routine''/)')
        SSOL(I)=CWHITM(SZ(I),SKA(I),SMU(I))
        SERROR(I)=ABS( (SSOL(I)-SEXACT(I))/SSOL(I) )
         WRITE(LOUT,'('' Calculated   '',2X,2F15.8)') SSOL(I)
         WRITE(LOUT,'('' Exact        '',2X,2F15.8)') SEXACT(I)
         WRITE(LOUT,'('' Rel error    '',4X,1P,D10.1)') SERROR(I)
         SERRMAX=MAX( SERRMAX,SERROR(I) )
         LTEST= LTEST .AND. SERRMAX .LE. STSTERR
#endif
 100  CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
       SOL(NT)=CWHITM((-.5E0,0E0),(.5E0,.5E0),(.5E0,.5E0))
       SOL(NT)=CWHITM((.5E0,.5E0),(.5E0,.5E0),(-.5E0,0E0))
#endif
#if defined(CERNLIB_DOUBLE)
      SSOL(NT)=CWHITM((-.5E0,0E0),(.5E0,.5E0),(.5E0,.5E0))
      SSOL(NT)=CWHITM((.5E0,.5E0),(.5E0,.5E0),(-.5E0,0E0))
      SOL(NT)=WWHITM((-.5D0,0D0),(.5D0,.5D0),(.5D0,.5D0))
      SOL(NT)=WWHITM((.5D0,.5D0),(.5D0,.5D0),(-.5D0,0D0))
#endif
      WRITE(LOUT,'(1X)')

C     Check if the test was successful
      IRC=ITEST('C328',LTEST)
      CALL PAGEND('C328')
      RETURN
      END
