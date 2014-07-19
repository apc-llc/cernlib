*
* $Id: gexin.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: gexin.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GEXIN(ICH,XST,IXST,ALST,ALST1,X,XM,PX,PY)
C     ****************************************************
C-- GENERATES ONE INITIAL PARTON
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/entrev.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
#include "cojets/tabqrk.inc"
      ICHR=ICH
      IF(ICH.EQ.6) ICHR=5
      CALL FINDX(ICHR,IXST,X,IX)
      IF(IX.GT.IXST) GO TO 300
      F2X=F2XQV(ICHR)
105   X=EXP(CJRN(X)*(ALST1-ALST)+ALST)
      IF(CJRN(X).GT.F2IN(X,ICHR)/F2X) GO TO 105
300   PT2=-PT2INT*LOG(CJRN(0))
      XM=PT2/(X*S)
      IF(XM.GT.X) GO TO 300
      PT=SQRT(PT2)
      PHI=PI2*CJRN(0)
      PX=PT*COS(PHI)
      PY=PT*SIN(PHI)
      RETURN
      END
