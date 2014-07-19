*
* $Id: wpsqz.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: wpsqz.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WPSQZ(XPSQ,BP1SQ,EL,PSQ,Z)
C     *************************************
C------------------------------------------------------------------
C-- GENERATES PSQ (OF PARENT) AND Z (LEPTON FRACTION) IN BRANCHING
C-- CALLED BY WRAD
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#if defined(CERNLIB_SINGLE)
      REAL CJRN,P,PJTOT,W,WL
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION CJRN,P,PJTOT,W,WL
#endif
#include "cojets/wcom.inc"
C
C-- GENERATE PSQ
      GPSQ=XPSQ
      IF(GPSQ.LT.QSQMN) GO TO 50
10    RR=CJRN(0.)
      IF(RR.LT.(QSQMN/GPSQ)**(1./TAU)) GO TO 15
      PSQ=GPSQ*RR**TAU
      IF(PSQ.LT.BP1SQ) GO TO 15
      IF(PSQ.LT.QSQMN) GO TO 50
      GO TO 16
15    IF(BP1SQ.LE.QSQMN) GO TO 50
      PSQ=BP1SQ
16    CONTINUE
C-- GENERATE Z
20    Z=1.-EPSI**CJRN(0.)
      WFUN=(1.+Z**2)/2.
      IF(CJRN(0.).GT.WFUN) GO TO 20
C-- CHECK WITH Z LIMITS -- IF NOT, EVOLVE FURTHER
      IF(BP1SQ.LT.-4.*S) GO TO 60
      ZLW=1.-(PSQ-ALSQ)/(2.*EL*(EL-SQRT(EL**2-PSQ)))
      IF(Z.LT.ZLW) GO TO 40
      RETURN
C-- Z CHECK FOR 1ST EMISSION
60    PCM=(S-PSQ)/(2.*SQRT(S))
      EL=SQRT(PSQ+PCM**2)
      ZLW=1.-(PSQ-ALSQ)/(2.*EL*(EL-PCM))
      ZUP=1.-(PSQ-ALSQ)/(2.*EL*(EL+PCM))
      IF(Z.LT.ZLW) GO TO 40
      IF(Z.GT.ZUP) GO TO 40
      RETURN
40    GPSQ=PSQ
      GO TO 10
C-- LEG STOPS -- NO BRANCHING
50    PSQ=ALSQ
      RETURN
      END
