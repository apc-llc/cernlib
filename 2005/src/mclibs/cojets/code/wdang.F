*
* $Id: wdang.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: wdang.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WDANG(THETA)
C     ***********************
C--  GENERATES THETA ANGLE IN W DECAYS
C-- THETA IS ANGLE BETWEEN FINAL QUARK, OR NEGATIVE LEPTON, OR NEUTRINO
C-- AND INITIAL QUARK
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boflag.inc"
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/itapes.inc"
#include "cojets/weakon.inc"
C
      DATA ICALL/0/
C
      IF(ICALL.GT.0) GO TO 5
      ICALL=1
      BM2=PMAS(3)**2
      PCM2=(BM2-(QMAS(6)+QMAS(5))**2)*(BM2-(QMAS(6)-QMAS(5))**2)
     *     /(4.*BM2)
      PCM=SQRT(ABS(PCM2))
      ET=SQRT(QMAS(6)**2+PCM2)
      EB=SQRT(QMAS(5)**2+PCM2)
      AM2X=(ET+PCM)*(EB+PCM)
5     CONTINUE
C
      IF(IBOFLA.EQ.1) GO TO 10
      IFBOS=PBOS(6)
      IF(ICHDB.NE.18) THEN
        C=2.*CJRN(0)**(1./3.)-1.
      ELSE IF(MOPTWZ.EQ.0) THEN
1       C=-1.+2.*CJRN(0)
        AM2=(ET+PCM*C)*(EB+PCM*C)
        IF(AM2X*CJRN(0).GT.AM2) GO TO 1
      ELSE
        BM2=PBOS(5)**2
        PCM2=(BM2-(QMAS(6)+QMAS(5))**2)*(BM2-(QMAS(6)-QMAS(5))**2)
     *       /(4.*BM2)
        PCM=SQRT(ABS(PCM2))
        ET=SQRT(QMAS(6)**2+PCM2)
        EB=SQRT(QMAS(5)**2+PCM2)
        AM2X=(ET+PCM)*(EB+PCM)
2       C=-1.+2.*CJRN(0)
        AM2=(ET+PCM*C)*(EB+PCM*C)
        IF(AM2X*CJRN(0).GT.AM2) GO TO 2
      ENDIF
      IF(IQRKFW.LT.0.) C=-C
      IF(IFBOS.EQ.4) C=-C
      THETA=ACOSX(C)
C
      RETURN
10    THETA=ACOSX(CTHWRF)
      RETURN
      END
