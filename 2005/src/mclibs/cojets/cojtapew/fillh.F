*
* $Id: fillh.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: fillh.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FILLH
C     ****************
C-- INTERFACES COJETS/COJTAPEW TO WRITE EVENT RECORD
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/boson.inc"
#include "cojets/data1.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetpar.inc"
#include "cojets/jetset.inc"
#include "cojets/keys.inc"
#include "cojets/khadro.inc"
#include "cojets/pfirst.inc"
#include "cojets/pinits.inc"
#include "cojets/pjets.inc"
#include "cojets/pmbias.inc"
#include "cojets/pthard.inc"
#include "cojets/qcds.inc"
#include "cojets/total.inc"
#include "cojets/zevel.inc"
C
C-- CALL BOOKH (AND WRBEG) BEFORE WRITING 1ST EVENT
      DATA ICALL/0/
C
      IF(ICALL.EQ.0) THEN
        ICALL=1
        CALL BOOKH
      ENDIF
C
C-- /IDRUN/
      IEVT=IEVT+1
C
      IF(LINV(1).EQ.0) GO TO 31
C-- /PFIRST/
      DO 22 I=1,2
      FSIGN=1.
      IF(I.EQ.2) FSIGN=-1.
      PFIRST(1,I)=PXINGV(I,1)
      PFIRST(2,I)=PYINGV(I,1)
      PFIRST(3,I)=.5*XINGV(I,1)*ECM*FSIGN
      PFIRST(4,I)=SQRT(PFIRST(1,I)**2+PFIRST(2,I)**2+PFIRST(3,I)**2)
      PFIRST(5,I)=0.
      IF(IFINGV(I,1).NE.0) IDFRST(I)=IFINGV(I,1)
      IF(IFINGV(I,1).EQ.0) IDFRST(I)=IDGL
22    CONTINUE
C-- /PINITS/
      DO 21 I=1,2
      FSIGN=1.
      IF(I.EQ.2) FSIGN=-1.
      PINITS(1,I)=PXACTV(I,1)
      PINITS(2,I)=PYACTV(I,1)
      PINITS(3,I)=.5*XACTV(I,1)*ECM*FSIGN
      PINITS(4,I)=SQRT(PINITS(1,I)**2+PINITS(2,I)**2+PINITS(3,I)**2)
      PINITS(5,I)=0.
      IF(IFACTV(I,1).NE.0) IDINIT(I)=IFACTV(I,1)
      IF(IFACTV(I,1).EQ.0) IDINIT(I)=IDGL
21    CONTINUE
C-- /PJETS/
      DO 30 LJ=1,3
      DO 30 J=1,5
30    PJETS(J,LJ)=0.
      DO 36 JT=1,2
36    IDJETS(JT)=0
      IF(LFINV(1).EQ.0) GO TO 35
      DO 10 JT=1,2
      IF(KHADRO.EQ.1) THEN
      DO 11 I=1,NPART
      JTI=ABS(IORIG(I))/IPACK
      IF(JTI.NE.JT) GO TO 11
      IF(IDCAY(I).NE.0) GO TO 11
      DO 13 J=1,4
13    PJETS(J,JT)=PJETS(J,JT)+PARHAD(I,J)
11    CONTINUE
      ELSE
        DO 12 I=1,NJSET
        JTI=ABS(JORIG(I))/JPACK
        IF(JTI.NE.JT) GO TO 12
        IF(JDCAY(I).NE.0) GO TO 12
        DO 14 J=1,4
14      PJETS(J,JT)=PJETS(J,JT)+PJSET(J,I)
12      CONTINUE
      ENDIF
      PJETS(5,JT)=SQRT(ABS(PJETS(4,JT)**2-PJETS(1,JT)**2
     1                     -PJETS(2,JT)**2-PJETS(3,JT)**2))
      IF(LEV(1).EQ.1.OR.(LEV(1).EQ.2.AND.LFINV(1).GT.6).OR.
     1(LEV(1).EQ.3.AND.LFINV(1).GT.3)) THEN
        IF(IFLFV(JT,1).NE.0) IDJETS(JT)=IFLFV(JT,1)
        IF(IFLFV(JT,1).EQ.0) IDJETS(JT)=IDGL
      ELSE
        IDJETS(JT)=IDENTF(IFLFV(JT,1))
      ENDIF
10    CONTINUE
35    CONTINUE
      IF(LEV(1).GE.2.AND.LEV(1).LE.4) THEN
        DO 15 J=1,4
15      QWJET(J)=P4LABV(J,1)
        QWJET(5)=SQRT(SHV(1))
        IDENTW=IDENTF(INT(PBOS(6)))
      ENDIF
C-- /JETPAR/
      IF(LFINV(1).NE.0) GO TO 5
      DO 4 I=1,2
      P(I) =0.
      PT(I)=0.
      YJ(I)=0.
      PHI(I)=0.
      XJ(I)=0.
      CTH(I)=0.
      STH(I)=0.
      TH(I)=0.
      JETTYP(I)=0.
4     CONTINUE
      GO TO 6
5     CONTINUE
      DO 1 I=1,2
      P(I) =SQRT(PJETS(1,I)**2+PJETS(2,I)**2+PJETS(3,I)**2)
      PT(I)=SQRT(PJETS(1,I)**2+PJETS(2,I)**2)
      YJ(I)=.5*LOG((PJETS(4,I)+PJETS(3,I))/(PJETS(4,I)-PJETS(3,I)))
      PJETS1=PJETS(1,I)
      PJETS2=PJETS(2,I)
      PHI(I)=ATAN2X(PJETS2,PJETS1)
      XJ(I)=2.*ABS(PJETS(3,I))/ECM
      CTH(I)=PJETS(3,I)/P(I)
      STH(I)=PT(I)/P(I)
      TYPCTH=CTH(I)
      TH(I)=ACOSX(TYPCTH)
      IF(LEV(1).EQ.1.OR.(LEV(1).EQ.2.AND.LFINV(1).GT.6).OR.
     1(LEV(1).EQ.3.AND.LFINV(1).GT.3)) THEN
        JETTYP(I)=ISJTQF(IFLFV(I,1))
      ELSE
        JETTYP(I)=ISJTPF(IFLFV(I,1))
      ENDIF
1     CONTINUE
6     CONTINUE
      SHAT=SHV(1)
      EIN1=.5*XACTV(1,1)*ECM
      THAT=AMV(1,1)**2-2.*((PFLABV(4,1,1)-PFLABV(3,1,1))*EIN1
     1     -PFLABV(1,1,1)*PXACTV(1,1)-PFLABV(2,1,1)*PYACTV(1,1))
      UHAT=AMV(2,1)**2-2.*((PFLABV(4,2,1)-PFLABV(3,2,1))*EIN1
     1     -PFLABV(1,2,1)*PXACTV(1,1)-PFLABV(2,2,1)*PYACTV(1,1))
      QSQ=QSQV(1)
      PTFFF=PTHARD
      ALFQSQ=1./(BALPH*LOG(QSQ/ALAMB**2)**2)
      X1=XACTV(1,1)
      X2=XACTV(2,1)
      DO 40 I=1,2
      PTA(1,I)=PXACTV(I,1)
      PTA(2,I)=PYACTV(I,1)
      XI1=XINGV(1,1)
      XI2=XINGV(2,1)
      PTI(1,I)=PXINGV(I,1)
      PTI(2,I)=PYINGV(I,1)
40    CONTINUE
      PBEAM(1)=.5*(1.-XINGV(1,1))*ECM
      PBEAM(2)=.5*(1.-XINGV(2,1))*ECM
      IF(LEV(1).GE.2.AND.LEV(1).LE.4) THEN
        QMW=SQRT(SHV(1))
        QW=SQRT(ABS(SHV(1)-P4LABV(4,1)**2))
        QTW=SQRT(P4LABV(1,1)**2+P4LABV(2,1)**2)
      YW=.5*LOG((P4LABV(4,1)+P4LABV(3,1))/(P4LABV(4,1)-P4LABV(3,1)))
        XW=2.*P4LABV(3,1)/ECM
        CTHW=P4LABV(3,1)/QW
        STHW=QTW/QW
        TYPCTH=CTHW
        THW=ACOSX(TYPCTH)
        QTMW=SQRT(QTW**2+QMW**2)
        P4LAB1=P4LABV(1,1)
        P4LAB2=P4LABV(2,1)
        PHIW=ATAN2X(P4LAB2,P4LAB1)
        SHAT1=SHAT
        THAT1=THAT
        UHAT1=UHAT
        JWTYP=ISBOSF(LINV(1))
        Q0W=P4LABV(4,1)
      ENDIF
      DO 2 I=1,2
      IACTYP(I)=ISJTQF(IFACTV(I,1))
2     INITYP(I)=ISJTQF(IFINGV(I,1))
      IF(KHADRO.EQ.1) THEN
      DO 3 J=1,5
3     PBEAMS(J)=PMBIAS(J)
      ELSE
        DO 16 J=1,5
16      PBEAMS(J)=0.
      ENDIF
31    CONTINUE
      IF(LEV(1).GE.2.AND.LEV(1).LE.4) THEN
        ISIGS=100*JWTYP+LFINV(1)
      ELSE
        ISIGS=100*LINV(1)+LFINV(1)
      ENDIF
C
      CALL WRTAPE
      RETURN
      END
