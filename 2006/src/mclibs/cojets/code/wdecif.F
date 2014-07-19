*
* $Id: wdecif.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: wdecif.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WDECIF(QES,QNS,QKS)
C     ******************************
C-- INTERFACES WDECAY
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/berend.inc"
#include "cojets/itapes.inc"
#include "cojets/photbe.inc"
#include "cojets/photon.inc"
#include "cojets/prmswd.inc"
      DIMENSION QES(4),QNS(4),QKS(4)
      DOUBLE PRECISION Q0
      DOUBLE PRECISION QE(4),QN(4),QK(4)
      DOUBLE PRECISION S,XMW,XM,XGWTOT,ALFA
      S=BES
      XMW=BEXMW
      XM=BEXM
      XGWTOT=BEXGWT
      ALFA=BEALFA
      Q0=BEQ0
      CALL WDECAY(QE,QN,QK)
      DO 10 J=1,4
      QES(J)=QE(J)
      QNS(J)=QN(J)
10    QKS(J)=QK(J)
      RETURN
      END
