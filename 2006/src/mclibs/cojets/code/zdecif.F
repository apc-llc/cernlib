*
* $Id: zdecif.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: zdecif.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ZDECIF(QPS,QMS,QKS)
C     ******************************
C-- INTERFACES ZDECAY
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/berend.inc"
#include "cojets/itapes.inc"
#include "cojets/parram.inc"
#include "cojets/photbe.inc"
#include "cojets/photon.inc"
      DIMENSION QPS(4),QMS(4),QKS(4)
      DOUBLE PRECISION Q0
      DOUBLE PRECISION QP(4),QM(4),QK(4)
      DOUBLE PRECISION S,XM,CVL,CAL,CVQ,CAQ,QQ,XMZ,XGZ,ALFA
      S=BES
      XM=BEXM
      CVL=BECVL
      CAL=BECAL
      CVQ=BECVQ
      CAQ=BECAQ
      QQ=BEQQ
      XMZ=BEXMZ
      XGZ=BEXGZ
      ALFA=BEALFA
      Q0=BEQ0
      CALL ZDECAY(QP,QM,QK)
      DO 10 J=1,4
      QPS(J)=QP(J)
      QMS(J)=QM(J)
10    QKS(J)=QK(J)
      RETURN
      END
