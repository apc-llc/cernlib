*
* $Id: bgexin.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bgexin.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BGEXIN(XST,IXST,ALST,ALST1,X,IX,IMODE)
C     *************************************************
C-- GENERATES ONE INITIAL PARTON -- CALLED BY BGPAIR
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/botqrk.inc"
#include "cojets/bxgen.inc"
#include "cojets/itapes.inc"
C
      IF(IMODE.EQ.2) GO TO 50
      IX=INT(NBIN*CJRN(1))+1
      IF(CJRN(2).GT.TBXAL(IX)) IX=LBXAL(IX)
      X=DBIN*FLOAT(IX-1)
      RETURN
50    CONTINUE
      IF(IX.EQ.IXST) GO TO 60
      X=X-DBIN*CJRN(3)
      RETURN
60    CONTINUE
105   X=EXP(CJRN(X)*(ALST1-ALST)+ALST)
      F2R=(F2IN(X,6)+F2IN(X,7))/F2X
      IF(CJRN(X+1).GT.F2R) GO TO 105
      RETURN
      END
