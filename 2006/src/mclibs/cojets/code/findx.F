*
* $Id: findx.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: findx.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FINDX(ICH,IXST,X,IX)
C     *******************************
C-- X  TABLE SEARCHING
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/tabqrk.inc"
      INTEGER IHALF(6)
      DATA IHALF/32,16,8,4,2,1/
      VST=0.
      IF(IXST.GT.1) VST=TABQRK(IXST-1,ICH)
      VR=VST+(TABQRK(NBIN,ICH)-VST)*CJRN(0)
      IC=64
      DO 10 IH=1,6
      ICL=IC
      ID=IHALF(IH)
      IF(VR.LT.TABQRK(IC,ICH)) ID=-ID
      IC=ICL+ID
10    CONTINUE
      ICL=IC
      ID=1
      IF(VR.LT.TABQRK(IC,ICH)) ID=-1
      IC=ICL+ID
      IX=MIN(IC,ICL)
      IF(IX.EQ.IXST) RETURN
      X1=FLOAT(IC-1)*DBIN
      X2=FLOAT(ICL-1)*DBIN
      X=((VR-TABQRK(IC,ICH))*X2+(TABQRK(ICL,ICH)-VR)*X1)
     1 /(TABQRK(ICL,ICH)-TABQRK(IC,ICH))
      RETURN
      END
