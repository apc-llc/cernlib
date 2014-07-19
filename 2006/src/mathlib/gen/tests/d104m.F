*
* $Id: d104m.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d104m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D104M
#include "gen/imp64.inc"
      PARAMETER ( TSTERR=1D-11)
      PARAMETER (A0 = 0, A1 = 1)
      EXTERNAL FD104
#include "iorc.inc"

      CALL HEADER('D104',0)

      WRITE(LOUT,'(/8X,''A'',9X,''B'',9X,''S'',15X,''D104'',25X,
     +''TEST'',7X,''Error'')')
      ERRMAX=0
      EPS=1D-12
      S=1D0
      WRITE(LOUT,'(1X)')
      DO 1 I = 0,30
      A=0
      B=0.1D0*I
      IF(A .EQ. 0 .AND. B .EQ. 1) GOTO 1
#if defined(CERNLIB_DOUBLE)
      R=DCAUCH(FD104,A,B,S,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=CAUCHY(FD104,A,B,S,EPS)
#endif
      T=0
      IF(A .NE. S .AND. B .NE. S) T=B-A+LOG(ABS((B-1)/(A-1)))
      ERRMAX= MAX( ERRMAX,ABS(R-T))
      WRITE(LOUT,'(3F10.1,2F25.15,1P,D10.1)') A,B,S,R,T,ABS(R-T)
    1 CONTINUE

      WRITE(LOUT,'(1X)')
      DO 2 I = 0,30
      A=0.1D0*I
      B=0.0D0
      IF(A .EQ. 1 .AND. B .EQ. 0) GOTO 2
#if defined(CERNLIB_DOUBLE)
      R=DCAUCH(FD104,A,B,S,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=CAUCHY(FD104,A,B,S,EPS)
#endif
      T=0.0D0
      IF(A .NE. S .AND. B .NE. S) T=B-A+LOG(ABS((B-1)/(A-1)))
      ERRMAX= MAX( ERRMAX,ABS(R-T))
      WRITE(LOUT,'(3F10.1,2F25.15,1P,D10.1)') A,B,S,R,T,ABS(R-T)
    2 CONTINUE
      WRITE(LOUT,'(/'' Largest Error:'',1P,D10.1)') ERRMAX
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      R=DCAUCH(FD104,A0,A1,S,EPS)
      R=DCAUCH(FD104,A1,A0,S,EPS)
#endif
#if !defined(CERNLIB_DOUBLE)
      R=CAUCHY(FD104,A0,A1,S,EPS)
      R=CAUCHY(FD104,A1,A0,S,EPS)
#endif
      WRITE(LOUT,'(1X)')
C     Check if the test was successful
      IRC=ITEST('D104',ERRMAX .LE. TSTERR)
      CALL PAGEND('D104')
      RETURN
      END
      FUNCTION FD104(X)
#include "gen/imp64.inc"
      FD104=X/(X-1)
      RETURN
      END
