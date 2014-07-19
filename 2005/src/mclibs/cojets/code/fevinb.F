*
* $Id: fevinb.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: fevinb.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FEVINB
C     *****************
C-- STORES INFORMATION IN /EVINT/ -- W/Z, DRELL-YAN FORMATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/entrev.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/itapes.inc"
#include "cojets/parq.inc"
#include "cojets/qcds.inc"
C
      LEVTYP=IEVTYP
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
      SHV(1)=PBOS(5)**2
      QSQV(1)=SHV(1)
      THV(1)=0.
10    CONTINUE
      LEV(1)=LEVTYP
      LINV(1)=PBOS(6)
      IF(LEVTYP.EQ.2) LFINV(1)=ICHDB-IDB(LINV(1))+1
      IF(LEVTYP.EQ.3) LFINV(1)=ICHDB-IDB(LINV(1))+1
      IF(LEVTYP.EQ.4) LFINV(1)=ICHDB
      DO 11 M=1,4
11    P4LABV(M,1)=PBOS(M)
C- REMAINING ENTRIES OF /EVINT/ ARE SET IN OTHER SUBRS. (AT THE SOURCE)
      RETURN
      END
