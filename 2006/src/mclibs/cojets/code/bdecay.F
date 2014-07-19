*
* $Id: bdecay.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: bdecay.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BDECAY
C     *****************
C-- HANDLES INTERMEDIATE BOSON DECAY IN PARTON CMS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/evint.inc"
#include "cojets/iflghv.inc"
#include "cojets/itapes.inc"
#include "cojets/jetnpc.inc"
#include "cojets/parq.inc"
#include "cojets/qcds.inc"
#include "cojets/quaor2.inc"
#include "cojets/radlep.inc"
#include "cojets/top.inc"
#include "cojets/weakon.inc"
#include "cojets/zpar2.inc"
      DATA ZH,WH/2.,3./,DH/4./
C
      IFBOS=PBOS(6)
      IQRKFW=PARACT(1,1,1)
C
C-- WEAK BOSON DECAY MODE
      IF(IFDC.GT.0) GO TO 50
      IF(WEAKON.EQ.DH) GO TO 51
5     CONTINUE
      TBR=CJRN(0)
      IDC=IDB(IFBOS)-1
10    IDC=IDC+1
      IF(MOPTWZ.EQ.0.AND.TBR.GT.CBR(IDC)) GO TO 10
      IF(MOPTWZ.EQ.1.AND.TBR.GT.CBR1(IDC)) GO TO 10
      GO TO 60
51    IDC=1.+3.*CJRN(0.)
      GO TO 60
50    IDC=IFDC
60    ICHDB=IDC
C
C-- CALL GENERATION OF FINAL STATE (ALONG Z AXIS)
      IF(WEAKON.EQ.ZH.AND.ICHDB.LE. 3) GO TO 70
      IF(WEAKON.EQ.ZH.AND.ICHDB.LE. 6) GO TO 71
      IF(WEAKON.EQ.WH.AND.ICHDB.LE.15) GO TO 70
      IF(WEAKON.EQ.DH) GO TO 70
      GO TO 80
70    IF(IRADLP.GT.0) GO TO 100
71    CALL BLEPT
      GO TO 90
80    CONTINUE
      IQUAO1=1
      IQUAO2=2
      CALL BJETS
      IF(IFLGHV.EQ.1) RETURN
90    CONTINUE
C
C
C-- APPROPRIATE ROTATION OF DECAY PRODUCTS
      IF(IFBOS.EQ.2) CALL ZDANG(THETA)
      IF(IFBOS.EQ.3.OR.IFBOS.EQ.4) CALL WDANG(THETA)
      IF(IFBOS.EQ.1) THEN
        IF(MOPTWZ.EQ.0) CALL DDANG(THETA)
        IF(MOPTWZ.EQ.1) CALL ZDANG(THETA)
      ENDIF
      PHI=PI2*CJRN(1)
      COSCMV(1)=COS(THETA)
      PHIV(1)=PHI
      CALL EDITP(NP)
C
      PCM=PFLABV(3,1,1)
      PFLABV(1,1,1)=PCM*SIN(THETA)*COS(PHI)
      PFLABV(2,1,1)=PCM*SIN(THETA)*SIN(PHI)
      PFLABV(3,1,1)=PCM*COS(THETA)
      PFLABV(1,2,1)=PCM*SIN(PI-THETA)*COS(PHI+PI)
      PFLABV(2,2,1)=PCM*SIN(PI-THETA)*SIN(PHI+PI)
      PFLABV(3,2,1)=-PCM*COS(THETA)
C
      RETURN
C
100   CALL BRADLP
      RETURN
      END
