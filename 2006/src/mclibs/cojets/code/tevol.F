*
* $Id: tevol.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: tevol.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE TEVOL(IBS)
C     *********************
C-- HANDLES EVOLUTION OF TIMELIKE QUANTA DUE TO INITIAL BREMSSTRAHLUNG
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/inmat.inc"
#include "cojets/isjetn.inc"
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/nevol.inc"
#include "cojets/nquaz.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/quaor.inc"
#include "cojets/rotq.inc"
#include "cojets/transl.inc"
      DIMENSION SETIN(6)
C
      IB=IBS
      IF(IB.EQ.1) THEN
        NQUA=NQUAZ
        NJSET=NJSETZ
      ENDIF
100   CONTINUE
      IF(IB.EQ.2) NQUA=NQUA1
      IF(IB.EQ.2) NJSET=NJSET1
      JETN=IB+10
      NJ=0
      MBK=PARACT(1,2,IB)
      MBKS=MBK+1
5     IF(MBK.EQ.0) GO TO 8
      NJ=NJ+1
C--  EXTRA PJSET INFO
      NJSET=NJSET+1
      IF(NJSET.GT.MXJSET) GO TO 560
      PJSET(1,NJSET)=PARACT(MBKS,1,IB)
      PJSET(2,NJSET)=PARACT(MBKS,2,IB)
      PJSET(5,NJSET)=PARACT(MBKS,5,IB)
      PMT2=PJSET(1,NJSET)**2+PJSET(2,NJSET)**2
      EPP=PARACT(MBKS,3,IB)
      EPM=PMT2/EPP
      PJSET(3,NJSET)=.5*(EPP-EPM)
      IF(IB.EQ.2) PJSET(3,NJSET)=-PJSET(3,NJSET)
      PJSET(4,NJSET)=.5*(EPP+EPM)
      JTYPE(NJSET)=JDENTF(INT(PARACT(MBKS,6,IB)))
      IF(NJ.EQ.1) JDCAY(NJSET)=0
      NJSETS=NJSET
C
      DO 7 L=1,6
7     SETIN(L)=PARSPE(MBK,L,IB)
      IF(INT(SETIN(6)).EQ.LGLUS) SETIN(6)=LGLUT
      IROT=IB
      IQUAOR=-IB
      CALL JETQCD(SETIN)
C--  EXTRA PJSET INFO
      JORIG(NJSETS)=JPACK*JETN+NJSET+1
      JORIG(NJSETS+1)=JORIG(NJSETS)
      JDCAY(NJSET+1)=JPACK*NJSETS+NJSETS+1
C
      MBK=PARSPE(MBK,4,IB)
      MBKS=MBK+1
      GO TO 5
8     IF(IB.EQ.1) NQUA1=NQUA
C--  EXTRA PJSET INFO
      NJSET=NJSET+1
      IF(IB.EQ.1) NJSET1=NJSET
      IF(NJSET.GT.MXJSET) GO TO 560
      PJSET(1,NJSET)=PXING(IB)
      PJSET(2,NJSET)=PYING(IB)
      PJSET(5,NJSET)=-QZEV
      PMT2=PJSET(1,NJSET)**2+PJSET(2,NJSET)**2
      EPP=XING(IB)*ECM
      EPM=PMT2/EPP
      PJSET(3,NJSET)=.5*(EPP-EPM)
      IF(IB.EQ.2) PJSET(3,NJSET)=-PJSET(3,NJSET)
      PJSET(4,NJSET)=.5*(EPP+EPM)
      JORIG(NJSET)=JPACK*JETN
      JTYPE(NJSET)=JDENTF(IFLING(IB))
      IF(NJ.EQ.0) JDCAY(NJSET)=0
C
      IB=IB+1
      IF(IB.GT.2) RETURN
      GO TO 100
C
C-- ABNORMAL EXIT
560   WRITE(ITLIS,561) MXJSET
561   FORMAT(5(/),1X,'NJSET EXCEEDS',I10,' (TEVOL)'
     1//1X,'INCREASE MXJSET')
      STOP
      END
