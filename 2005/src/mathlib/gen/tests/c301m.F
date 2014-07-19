*
* $Id: c301m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c301m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C301M

C     Routine to test MATHLIB routines FREQ and DFREQ (C301)

#include "imp64r.inc"
      REAL FREQ,SFFREQ,GAUSS
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

      LOGICAL LTEST
      EXTERNAL FFREQ,SFFREQ,GAUSS

#include "gen/def64.inc"
     + X,Z0,Z1
      PARAMETER (Z0 = 0, Z1 = 1)
      CHARACTER*(*) PNAME
      PARAMETER(PNAME='C301')
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOL/1D-4, 5D-13/


C     PI=4*ATAN(Z1)
      PI = 3.14159 26535 89793D0
      EPS=1D-15
      REPS=1E-7

      CALL HEADER(PNAME,0)
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR FREQ'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
      WRITE(LOUT,'(/10X,''TEST FOR FREQ'')')
      WRITE(LOUT,'(/9X,''X'',13x,''FREQ'',21X,''TEST'',
     +13X,''Error'')')
       ENDIF
      IF(JF.EQ.2)THEN
      WRITE(LOUT,'(/10X,''TEST FOR DFREQ'')')
      WRITE(LOUT,'(/9X,''X'',13x,''DFREQ'',21X,''TEST'',
     +13X,''Error'')')
       ENDIF
#endif
      ERRMAX =0D0
      DO 1 I = -80,80
      X=I/10D0
#if !defined(CERNLIB_DOUBLE)
      FQ=FREQ(X)
      U=10
      IF(X .LT. -5) U=15
      TQ=(1/SQRT(2*PI))*GAUSS(SFFREQ,-U,X,EPS*ABS(FQ))
      DRQ=0
      IF (FQ .NE. 0D0) DRQ=ABS((FQ-TQ)/FQ)
         ERRMAX= MAX( ERRMAX,DRQ)
      WRITE(LOUT,'(1X,F10.1,2D25.7,1P,E10.1)') X,FQ,TQ,DRQ
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX=X
                 RZ1=Z1
                 RFQ=FREQ(RX)
                 RU=10*RZ1
      RTQ=(1/SQRT(2*PI))*GAUSS(SFFREQ,-RU,RX,REPS*ABS(RFQ))
      RDRQ=0
      IF (RFQ .NE. 0E0) RDRQ=ABS((RFQ-RTQ)/RFQ)
                 X=RX
                 FQ=RFQ
                 TQ=RTQ
                 DRQ=RDRQ
        RERRMAX= ERRMAX
         ERRMAX= MAX(RERRMAX,RDRQ)
      WRITE(LOUT,'(1X,F10.1,2E25.7,1P,E10.1)') X,FQ,TQ,DRQ
      ENDIF
      IF(JF.EQ.2)THEN
      FQ=DFREQ(X)
      U=10
      IF(X .LT. -5) U=15
      TQ=(1/SQRT(2*PI))*DGAUSS(FFREQ,-U,X,EPS*ABS(FQ))
      DRQ=0
      IF (FQ .NE. 0D0) DRQ=ABS((FQ-TQ)/FQ)
         ERRMAX= MAX( ERRMAX,DRQ)
      WRITE(LOUT,'(1X,F10.1,2D25.7,1P,E10.1)') X,FQ,TQ,DRQ
      ENDIF
#endif
    1 CONTINUE

#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''Largest Relative Error was'',
     +1P,D10.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
1000  CONTINUE
C     Check if the test was successful
      IRC=ITEST('C301',LTEST)
      CALL PAGEND(PNAME)
      END
      FUNCTION FFREQ(T)
#include "gen/imp64.inc"

      FFREQ=EXP(-T**2/2)
      RETURN
      END
      FUNCTION SFFREQ(T)

      SFFREQ=EXP(-T**2/2)
      RETURN
      END
