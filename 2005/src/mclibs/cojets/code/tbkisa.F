*
* $Id: tbkisa.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: tbkisa.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE TBKISA(IFLAS,QSQS,XS,PXS,PYS)
C     ****************************************
C--  SUPPLEMENTS TBOOK FOR SPACELIKE QUANTA (USED FOR JETSET)
C--  PARACT(MBKS, , ) FOR MBKS.GT.1 USED FOR STORAGE
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/parq.inc"
      MBK=MBOOK(IBEAM)
      MBKS=MBK+1
      PARACT(MBKS,1,IBEAM)=PXS
      PARACT(MBKS,2,IBEAM)=PYS
      PARACT(MBKS,3,IBEAM)=XS*ECM
      PARACT(MBKS,5,IBEAM)=-SQRT(QSQS)
      PARACT(MBKS,6,IBEAM)=IFLAS
      RETURN
      END
