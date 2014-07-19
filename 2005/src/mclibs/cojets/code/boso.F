*
* $Id: boso.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: boso.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BOSO
C     ***************
C-- W AND Z BOSON (OR VIRTUAL DRELL-YAN PHOTON) KINEMATICS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boflag.inc"
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/iflghv.inc"
#include "cojets/inmat.inc"
#include "cojets/itapes.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/quaor.inc"
#include "cojets/rotq.inc"
#include "cojets/transl.inc"
#include "cojets/weakon.inc"
      DATA ZH,WH/2.,3./,DH/4./
C
      IBOFLA=0
      IFLA1=PARACT(1,1,1)
      XM1=PARACT(1,3,1)
      XP1=PARACT(1,5,1)
      PX1=PARACT(1,6,1)
      PY1=PARACT(1,7,1)
      IFLA2=PARACT(1,1,2)
      XM2=PARACT(1,3,2)
      XP2=PARACT(1,5,2)
      PX2=PARACT(1,6,2)
      PY2=PARACT(1,7,2)
C
      PBOS(5)=SQRT(BIM2)
      IF(MOPTWZ.EQ.0.AND.WIDTH.GT.1.E-3) CALL BWIDTH(PBOS(5))
      IF(MOPTWZ.EQ.1.OR.WEAKON.EQ.DH) PBOS(5)=SQRT(SHARD)
      PBOS(1)=PX1+PX2
      PBOS(2)=PY1+PY2
      PBOS(3)=(XP1-XP2-XM1+XM2)*ECM*.5
      PBOS(4)=SQRT(PBOS(5)**2+PBOS(1)**2+PBOS(2)**2+PBOS(3)**2)
C
      IF(WEAKON.EQ.ZH) PBOS(6)=2
      IF(WEAKON.EQ.DH) PBOS(6)=1
      ICHRG1=SIGN(ICHRGQ(ABS(IFLA1)),IFLA1)
      ICHRG2=SIGN(ICHRGQ(ABS(IFLA2)),IFLA2)
      IF(WEAKON.EQ.WH.AND.(ICHRG1+ICHRG2).GT.0) PBOS(6)=3
      IF(WEAKON.EQ.WH.AND.(ICHRG1+ICHRG2).LT.0) PBOS(6)=4
C
      IFBOS=PBOS(6)
      IF(IFDC.EQ.0) GO TO 100
      CALL BDECAY
      IF(IFLGHV.EQ.1) RETURN
      CALL FEVINB
      CALL BBOOST
C
      RETURN
C
100   CONTINUE
      NPART=NPART+1
      DO 1 J=1,6
1     PARHAD(NPART,J)=PBOS(J)
      PARHAD(NPART,7)=-1
      IORIG(NPART)=IPACK*1
      IDENT(NPART)=IDENTF(INT(PARHAD(NPART,6)))
      IDCAY(NPART)=0
      PCMV(1)=0.
      COSCMV(1)=0.
      PHIV(1)=0.
      AMV(1,1)=0.
      AMV(2,1)=0.
      IFLFV(1,1)=0
      IFLFV(2,1)=0
      DO 16 I=1,2
      DO 16 J=1,4
16    PFLABV(J,I,1)=0.
      ICHDB=IDB(INT(PBOS(6)))-1
      CALL FEVINB
C
      RETURN
      END
