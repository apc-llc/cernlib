*
* $Id: zgens.F,v 1.1.1.1 1996/01/11 14:14:45 mclareni Exp $
*
* $Log: zgens.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:45  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZGENS(IFUN,Z,IFLAQ)
C     ******************************
C-- GENERATES Z
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
C
      DIMENSION WGTMAX(3)
      DATA WGTMAX/2.,1.125,1./
      DATA ICALL/0/
C
      IF(ICALL.GT.0) GO TO 30
      ICALL=1
      ALLW=LOG(1.-ZLW)
C     LN(0.5)
      ALLWG=-.6931471
      ALUP=LOG(1.-ZUP)
      DZ=ALLW-ALUP
      DZG=ALLWG-ALUP
30    CONTINUE
C
      IF(IFUN.EQ.1) GO TO 1
      IF(IFUN.EQ.2) GO TO 2
      IF(IFUN.EQ.3) GO TO 3
C
C-- QUARK BRANCHING
1     CONTINUE
11    RR=CJRN(0.)
      Z1=EXP(RR*DZ+ALUP)
      Z=1.-Z1
      WFUN=1.+Z**2
      IF(CJRN(0.).GT.WFUN/WGTMAX(IFUN)) GO TO 11
      RETURN
C
C-- GLUON BRANCHING INTO TWO GLUONS
2     CONTINUE
21    RR=CJRN(0.)
      Z1=EXP(RR*DZG+ALUP)
      Z=1.-Z1
      A=Z**2
      B=Z1**2
      WFUN=(A+B+A*B)/Z
      IF(CJRN(0.).GT.WFUN/WGTMAX(IFUN)) GO TO 21
      IF(CJRN(0.).GT.0.5) Z=Z1
      RETURN
C
C-- GLUON BRANCHING INTO Q-QB
3     CONTINUE
      Z=ZLW+(ZUP-ZLW)*CJRN(0.)
      Z1=1.-Z
      WFUN=Z**2+Z1**2
      IF(CJRN(0.).GT.WFUN) GO TO 3
      IFLAQ=INT(NFLAVS*CJRN(0))+1
      RETURN
C
      END
