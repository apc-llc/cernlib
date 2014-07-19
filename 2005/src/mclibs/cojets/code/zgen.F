*
* $Id: zgen.F,v 1.1.1.1 1996/01/11 14:14:45 mclareni Exp $
*
* $Log: zgen.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:45  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZGEN(IFUN,ZLW,ZUP,FLAG,Z)
C     ************************************
C-- GENERATES Z
C
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION WGTMAX(3)
      DATA WGTMAX/2.,1.125,1./
C
      IF(FLAG.GT.0.) GO TO 100
      IF(IFUN.EQ.1) GO TO 1
      IF(IFUN.EQ.2) GO TO 2
      IF(IFUN.EQ.3) GO TO 3
C
C-- QUARK BRANCHING
1     CONTINUE
      ALLW=LOG(1.-ZLW)
      ALUP=LOG(1.-ZUP)
      DZ=ALLW-ALUP
11    RR=CJRN(0.)
      Z1=EXP(RR*DZ+ALUP)
      Z=1.-Z1
      WFUN=1.+Z**2
      IF(CJRN(0.).GT.WFUN/WGTMAX(IFUN)) GO TO 11
      RETURN
C
C-- GLUON BRANCHING INTO TWO GLUONS
2     CONTINUE
C     LOG(0.5)
      ALLW=-.6931471
      ALUP=LOG(1.-ZUP)
      DZ=ALLW-ALUP
21    RR=CJRN(0.)
      Z1=EXP(RR*DZ+ALUP)
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
      WFUN=Z**2+(1.-Z)**2
      IF(CJRN(0.).GT.WFUN) GO TO 3
      RETURN
C
100   Z=ZUP
      RETURN
      END
