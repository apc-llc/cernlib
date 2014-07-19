*
* $Id: fevinj.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: fevinj.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FEVINJ
C     *****************
C-- STORES INFORMATION IN /EVINT/ -- SINGLE TWO-BODY SCATTERING
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/entrev.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/imlf.inc"
#include "cojets/itapes.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/twobod.inc"
C
      LEVTYP=IEVTYP
      LEV(1)=LEVTYP
      IF(NQUA.EQ.0) GO TO 19
C-- HARD EVENT
      NMULP=1
      DO 10 J=1,2
      XINGV(J,1)=XING(J)
      PXINGV(J,1)=PXING(J)
      PYINGV(J,1)=PYING(J)
      IFINGV(J,1)=IFLING(J)
      IF(IFINGV(J,1).EQ.LGLUS) IFINGV(J,1)=0
      XACTV(J,1)=PARACT(1,5,J)
      PXACTV(J,1)=PARACT(1,6,J)
      PYACTV(J,1)=PARACT(1,7,J)
      IFACTV(J,1)=PARACT(1,1,J)
      IF(IFACTV(J,1).EQ.LGLUS) IFACTV(J,1)=0
10    CONTINUE
      LINV(1)=IM
      LFINV(1)=LF
      SHV(1)=SH
      QSQV(1)=QSQ
      THV(1)=TH
      COSCMV(1)=COSCM
      PHIV(1)=PHI
      PCMV(1)=PCM
      AMV(1,1)=Q1
      AMV(2,1)=Q2
      IFLFV(1,1)=IFLAF1
      IFLFV(2,1)=IFLAF2
      DO 11 M=1,4
      P4LABV(M,1)=PFTOT(M)
      PFLABV(M,1,1)=PF1(M)
11    PFLABV(M,2,1)=PF2(M)
      RETURN
C-- SOFT EVENT
19    CONTINUE
      NMULP=0
      DO 20 J=1,2
      XINGV(J,1)=0.
      PXINGV(J,1)=0.
      PYINGV(J,1)=0.
      IFINGV(J,1)=0
      XACTV(J,1)=0.
      PXACTV(J,1)=0.
      PYACTV(J,1)=0.
      IFACTV(J,1)=0
20    CONTINUE
      LINV(1)=0
      LFINV(1)=0
      SHV(1)=0.
      QSQV(1)=0.
      THV(1)=0.
      COSCMV(1)=0.
      PHIV(1)=0.
      PCMV(1)=0.
      AMV(1,1)=0.
      AMV(2,1)=0.
      IFLFV(1,1)=0
      IFLFV(2,1)=0
      DO 21 M=1,4
      P4LABV(M,1)=0.
      PFLABV(M,1,1)=0.
21    PFLABV(M,2,1)=0.
      RETURN
      END
