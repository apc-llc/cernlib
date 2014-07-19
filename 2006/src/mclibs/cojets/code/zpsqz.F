*
* $Id: zpsqz.F,v 1.1.1.1 1996/01/11 14:14:45 mclareni Exp $
*
* $Log: zpsqz.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:45  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZPSQZ(XPSQ,PSQ,Z,ZC)
C     *******************************
C------------------------------------------------------------------
C-- CALLED BY Z0RAD
C-- GENERATES PSQ (OF PARENT) AND Z (LEPTON FRACTION) IN BRANCHING
C------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#if defined(CERNLIB_SINGLE)
      REAL CJRN,P,PJTOT,W,WL
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION CJRN,P,PJTOT,W,WL
#endif
#include "cojets/zcom.inc"
C
C-- GENERATE PSQ
      GPSQ=XPSQ
      IF(GPSQ.LT.QSQMN) GO TO 50
10    RR=CJRN(0.)
      IF(RR.LT.(QSQMN/GPSQ)**(1./TAU)) GO TO 50
      PSQ=GPSQ*RR**TAU
      IF(PSQ.LE.QSQMN) GO TO 50
C-- GENERATE Z
20    ZC=EPSI**CJRN(0.)
      Z=1.-ZC
      WFUN=(1.+Z**2)/2.
      IF(CJRN(0.).GT.WFUN) GO TO 20
      ZCLWX=GMSQ/PSQ
      IF(ZC.LE.ZCLWX) GO TO 40
C-- FINAL CHECK WITH Z LIMITS -- IF NOT, EVOLVE FURTHER
      B2M=(1.+(ALSQ-GMSQ)/PSQ)/2.
      C=ALSQ/PSQ
      DISCR=B2M**2-C
      SQDIS=0.
      IF(DISCR.LT.0.) GO TO 30
      SQDIS=SQRT(DISCR)
30    ZLW=B2M-SQDIS
      ZUP=B2M+SQDIS
      IF(Z.LE.ZLW) GO TO 40
      IF(Z.GE.ZUP) GO TO 40
      RETURN
40    GPSQ=PSQ
      GO TO 10
C-- LEG STOPS -- NO BRANCHING
50    PSQ=ALSQ
      RETURN
      END
