*
* $Id: geqsq.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: geqsq.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GEQSQ(ALQ,IALQ)
C     **************************
C-- GENERATES HARD QSQ
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/forgen.inc"
#include "cojets/itapes.inc"
      INTEGER IHALF(6)
      DATA IHALF/32,16,8,4,2,1/
      VR=CJRN(0)
      IF(VR.LT.WGALQT(1)) GO TO 30
      IC=64
      DO 10 IH=1,6
      ICL=IC
      ID=IHALF(IH)
      IF(VR.LT.WGALQT(IC)) ID=-ID
      IC=ICL+ID
10    CONTINUE
      ICL=IC
      ID=1
      IF(VR.LT.WGALQT(IC)) ID=-1
      IC=ICL+ID
      IF(ABS(WGALQT(IC)-WGALQT(ICL)).LT.1.E-30) GO TO 20
      ALQ1=ALQI+DALQ*FLOAT(IC)
      ALQ2=ALQI+DALQ*FLOAT(ICL)
      ALQ=((VR-WGALQT(IC))*ALQ2+(WGALQT(ICL)-VR)*ALQ1)
     1 /(WGALQT(ICL)-WGALQT(IC))
      IALQ=MAX(IC,ICL)
      RETURN
20    IALQ=MIN(IC,ICL)
      ALQ=ALQI+DALQ*FLOAT(IALQ)
      RETURN
30    ALQ=ALQI+DALQ*VR/WGALQT(1)
      IALQ=1
      RETURN
      END
