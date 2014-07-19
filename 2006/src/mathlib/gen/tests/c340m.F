*
* $Id: c340m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c340m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C340M
#include "iorc.inc"
C     This program tests GENLIB routines EBSIR3 and EBSKR3 (C340) and
C     their equivalent double precision routines DEBIR3 and DEBKR3.
C     Numerical values calculated by the routines are compared with
C     values obtained by numerical integration, the integration is not
C     done here.
C          Written by T Hepworth, CN division, Cern, Meyrin, 19.6.90
C     Since values of K3 are even with respect to NU,
C     then  BSKR3(X,-NU)=BSKR3(X,NU)  and  DBSKR3(X,-NU)=DBSKR3(X,NU)
      INTEGER           NU(7)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION  X(7),I3(7),K3(7),IRES(7),KRES(7),DEBIR3,DEBKR3
      DOUBLE PRECISION  IERROR(7),KERROR(7),IMXERR,KMXERR,TSTERR
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL              X(7),I3(7),K3(7),IRES(7),KRES(7),EBSIR3,EBSKR3
      REAL              IERROR(7),KERROR(7),IMXERR,KMXERR,TSTERR
#endif
C     Specify the maximum allowed errors for the tests still to be
C     considered successful
      PARAMETER ( TSTERR=1D-10 )

C     Set up the test parameters
      DATA NU(1),X(1) /  1,  0.10 D00 /
      DATA NU(2),X(2) / -2,  2.00 D00 /
      DATA NU(3),X(3) / -1,  4.99 D00 /
      DATA NU(4),X(4) /  2,  5.00 D00 /
      DATA NU(5),X(5) /  1,  6.00 D00 /
      DATA NU(6),X(6) / -2,  8.00 D00 /
      DATA NU(7),X(7) / -1, 20.00 D00 /
C     Set up the values to test against (for the parameter values above)
C     Note these values are EXP(-X)*I(X) and EXP(X)*K(X)
      DATA I3(1),K3(1) /0.3739954918335082D+00, 0.3204805551950917D+01/
      DATA I3(2),K3(2) /0.2676629569922359D+00, 0.9224418385401156D+00/
      DATA I3(3),K3(3) /0.1814364652801527D+00, 0.5539553608988779D+00/
      DATA I3(4),K3(4) /0.1745283958635541D+00, 0.5705632049374417D+00/
      DATA I3(5),K3(5) /0.1649603834689937D+00, 0.5061963869675941D+00/
      DATA I3(6),K3(6) /0.1392218947048501D+00, 0.4482259428155665D+00/
      DATA I3(7),K3(7) /0.8952469043302264D-01, 0.2793012496549469D+00/

      CALL HEADER('C340',0)
      WRITE(LOUT,'(/6X,''X'',2X,''NU'',9X,''Calculated'',19X,
     +   ''Exact'',10x,''Absolute Error'')')

      IMXERR=0D0
      KMXERR=0D0
      DO 100 I=1,7
#if defined(CERNLIB_DOUBLE)
         IRES(I)=DEBIR3( X(I),NU(I) )
         KRES(I)=DEBKR3( X(I),NU(I) )
#endif
#if !defined(CERNLIB_DOUBLE)
         IRES(I)=EBSIR3( X(I),NU(I) )
         KRES(I)=EBSKR3( X(I),NU(I) )
#endif
         IERROR(I)=ABS ( IRES(I)-I3(I) )
         KERROR(I)=ABS ( KRES(I)-K3(I) )
         WRITE(LOUT,'(1X,F6.2,I4,2(/6X,2D24.16,F24.16))')
     +   X(I),NU(I),IRES(I),I3(I),IERROR(I),KRES(I),K3(I),KERROR(I)
C        Calculate maximum errors
         IMXERR=MAX ( IMXERR,IERROR(I) )
         KMXERR=MAX ( KMXERR,KERROR(I) )
100   CONTINUE

      WRITE(LOUT,'('' IMXERR='',F24.16,'' KMXERR='',F24.16)')IMXERR,
     +                                                KMXERR
C     Check if the test was successful
      IRC=ITEST('C340',IMXERR .LE. TSTERR .AND. KMXERR .LE. TSTERR)
      CALL PAGEND('C340')
      RETURN
      END
