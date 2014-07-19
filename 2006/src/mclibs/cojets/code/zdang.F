*
* $Id: zdang.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: zdang.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZDANG(THETA)
C     ***********************
C-- GENERATES THETA ANGLE IN Z DECAY
C-- THETA IS ANGLE BETWEEN FINAL QUARK, OR NEGATIVE LEPTON, OR NEUTRINO
C-- AND INITIAL QUARK
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/itapes.inc"
#include "cojets/weakon.inc"
#include "cojets/zpar2.inc"
#include "cojets/zwpar.inc"
      INTEGER LFV(12)
      DIMENSION CASYM(2,4)
      DIMENSION AT1(2),AT2(2),AT3(2),AM2X(2)
C
      DATA LFV/3*1,3*2,3,4,4,3,4,3/
      DATA ICALL/0/
C
      ASYM(A1,B1,A2,B2)=8.*A1*B1*A2*B2/((A1**2+B1**2)*(A2**2+B2**2))
C
      IF(ICALL.GT.0) GO TO 10
      ICALL=1
      XW=S2THW
      RHOT=(QMAS(6)/PMAS(2))**2
      XP=SQRT(ABS(1.-4.*RHOT))
C-- 1 FOR CH=2/3 QUARK OR LEPTON, 2 FOR CH=1/3 QUARK OR NEUTRINO
      DO 5 LI=1,2
      DO 6 LF=1,2
6     CASYM(LI,LF)=ASYM(AQ(LI),BQ(LI),AL(LF),BL(LF))
      DO 7 LF=3,4
7     CASYM(LI,LF)=ASYM(AQ(LI),BQ(LI),AQ(LF-2),BQ(LF-2))
      FI=AQ(LI)**2+BQ(LI)**2
      AT1(LI)=FI*(AQ(1)**2+BQ(1)**2+4.*RHOT*(AQ(1)**2-BQ(1)**2))
      AT2(LI)=8.*AQ(LI)*BQ(LI)*AQ(1)*BQ(1)*XP
      AT3(LI)=FI*(AQ(1)**2+BQ(1)**2)*XP**2
      AM2X(LI)=ABS(AT1(LI))+ABS(AT2(LI))+ABS(AT3(LI))
5     CONTINUE
10    CONTINUE
C
      LI=1
      IF(3.*CHARGE(ABS(IQRKFW)).LT.1.5) LI=2
      IF(MOPTWZ.EQ.0) THEN
        IF(ICHDB.NE.12) THEN
      LF=LFV(ICHDB-IDB(2)+1)
      CAS=CASYM(LI,LF)
20    C=2.*CJRN(1)-1.
      IF(CJRN(2).GT.(1.+C**2+CAS*C)/(2.+ABS(CAS))) GO TO 20
        ELSE
11        C=-1.+2.*CJRN(0)
          AM2=AT1(LI)+AT2(LI)*C+AT3(LI)*C**2
          IF(AM2X(LI)*CJRN(0).GT.AM2) GO TO 11
        ENDIF
      ELSE
        LXF=ICHDB
        RHO4=0.
        IF(LXF.EQ.12) RHO4=SZ(3)/SX
        FS1=SX*DS*DEN
        FS2=SX**2*DEN
        ABW1=SZF(1,LXI,LXF)*(1.+RHO4)
     *      +SZF(2,LXI,LXF)*(1.+RHO4)*FS1
     *      +(SZF(5,LXI,LXF)+RHO4*SZF(6,LXI,LXF))*FS2
        ABW2=SQRT(ABS(1.-RHO4))*(SZF(7,LXI,LXF)*FS1+SZF(8,LXI,LXF)*FS2)
        ABW3=(1.-RHO4)*(SZF(1,LXI,LXF)+SZF(2,LXI,LXF)*FS1
     *       +SZF(5,LXI,LXF)*FS2)
        ABWX=ABS(ABW1)+ABS(ABW2)+ABS(ABW3)
12      C=-1.+2.*CJRN(FS1)
        ABW=ABW1+ABW2*C+ABW3*C**2
        IF(ABWX*CJRN(FS2).GT.ABW) GO TO 12
      ENDIF
      IF(IQRKFW.LT.0) C=-C
      THETA=ACOSX(C)
C
      RETURN
      END
