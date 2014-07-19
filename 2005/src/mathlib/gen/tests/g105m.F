*
* $Id: g105m.F,v 1.1.1.1 1996/04/01 15:01:29 mclareni Exp $
*
* $Log: g105m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:29  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G105M
C     Routine to test MATHLIB routines GAUSIN and DGAUSN (G105)
#include "gen/def64.inc"
     +   DFREQ,DR,DP,DT,DGAUSN,ERRMAX,TSTERR
      LOGICAL LTEST
      DIMENSION TSTERR(2)
#include "iorc.inc"
      DATA TSTERR/1D-6,1D-14/
      CALL HEADER('G105',0)
      LTEST= .TRUE.
      ERRMAX= 0D0
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/8X,''P'',9X,''GAUSIN'',10X,''DFREQ'',10X,
     +''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/8X,''P'',12X,''GAUSIN'',21X,'' FREQ'',17X,
     +''Error'')')
#endif
      DO 1 IP = 5,995,5
      DP=0.001D0*IP
      P=DP
      R=GAUSIN(P)
#if defined(CERNLIB_DOUBLE)
      DT=DFREQ(DBLE(R))
#endif
#if !defined(CERNLIB_DOUBLE)
      DT= FREQ(R)
#endif
      ERRMAX=MAX(ERRMAX,ABS(DP-DT))
      WRITE(LOUT,'(1X,F10.3,3F15.7)') P,R,DT,DP-DT
    1 CONTINUE
      WRITE(LOUT,'(/''GAUSIN Largest Error:'',1P,D10.1)')ERRMAX
#if defined(CERNLIB_DOUBLE)
      LTEST= LTEST .AND. ERRMAX .LE. TSTERR(1)
#endif
#if !defined(CERNLIB_DOUBLE)
      LTEST= LTEST .AND. ERRMAX .LE. TSTERR(2)
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/8X,''P'',12X,''DGAUSN'',21X,''DFREQ'',17X,
     +''Error'')')
      ERRMAX= 0D0
      DO 2 IP = 2,998,2
      DP=0.001D0*IP
      DR=DGAUSN(DP)
      DT=DFREQ(DR)
      ERRMAX=MAX(ERRMAX,ABS(DP-DT))
      WRITE(LOUT,'(1X,F10.3,3F25.15)') DP,DR,DT,DP-DT
    2 CONTINUE
      WRITE(LOUT,'(/''DGAUSN Largest Error:'',1P,D10.1)')ERRMAX
      LTEST= LTEST .AND. ERRMAX .LE. TSTERR(1)
#endif
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      R=DGAUSN(3D0)
#endif
      R=GAUSIN(3.)
      WRITE(LOUT,'(1X,F15.7)') R
      IRC= ITEST('G105',LTEST)
      CALL PAGEND('G105')
      RETURN
      END
