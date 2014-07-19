*
* $Id: psqgen.F,v 1.1.1.1 1996/01/11 14:14:41 mclareni Exp $
*
* $Log: psqgen.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:41  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE PSQGEN(IFLA,XPSQ,PSQ,IGO)
C     ************************************
C-- GENERATES SQUARE MASS (PSQ) OF LEG
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/itapes.inc"
#include "cojets/keybre.inc"
#include "cojets/m2qua.inc"
#include "cojets/tabpsq.inc"
C
      IF(KEYBRE.GE.2) GO TO 200
      KFLA=KODTAB(IFLA)
      RIX=(LOG(XPSQ)-ALMIN(IFLA))/DSTEP(IFLA)+1.
      IF(RIX.LT.1.) GO TO 100
      IX=RIX
      DIX=RIX-FLOAT(IX)
      TBX=TABPSQ(KFLA,IX)+(TABPSQ(KFLA,IX+1)-TABPSQ(KFLA,IX))*DIX
      TBGEN=TBX+LOG(CJRN(0.))
C
      IF(TBGEN.LT.0.) GO TO 100
C
C-- LEG WILL CASCADE, GENERATE MASS
      IF(TBGEN.LT.TABPSQ(KFLA,IX)) GO TO 1
      PSQ=EXP((FLOAT(IX) + (TBGEN-TABPSQ(KFLA,IX))/
     1         (TABPSQ(KFLA,IX+1)-TABPSQ(KFLA,IX)) - 1.) * DSTEP(IFLA)
     2        +  ALMIN(IFLA))
      IGO=1
      RETURN
C
1     CONTINUE
      IUP=IX
      ILW=1
3     IF(IUP.EQ.ILW+1) GO TO 4
      IHALF=ILW+(IUP-ILW)/2
      IF(TBGEN.LT.TABPSQ(KFLA,IHALF)) GO TO 2
      ILW=IHALF
      GO TO 3
2     IUP=IHALF
      GO TO 3
4     PSQ=EXP((FLOAT(ILW) + (TBGEN-TABPSQ(KFLA,ILW))/
     1         (TABPSQ(KFLA,ILW+1)-TABPSQ(KFLA,ILW)) - 1.)
     2        * DSTEP(IFLA)  +  ALMIN(IFLA))
      IGO=1
      RETURN
C
C-- LEG STOPS, MASS SET EQUAL TO CUTOFF
100   CONTINUE
      PSQ=QZFLSQ(IFLA)
      IF(XPSQ.LT.PSQ) PSQ=XPSQ
      IGO=0
      RETURN
C
C-- RADIATION TURNED OFF
200   CONTINUE
      IFLAB=ABS(IFLA)
      IF(IFLA.EQ.LGLU) PSQ=0.
      IF(IFLA.NE.LGLU) PSQ=Q2QUA(IFLAB)
      IGO=0
      RETURN
      END
