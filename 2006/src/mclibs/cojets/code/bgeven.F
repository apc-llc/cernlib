*
* $Id: bgeven.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bgeven.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BGEVEN
C     *****************
C-- HANDLES W AND Z (OR DRELL-YAN) EVENT GENERATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/boends.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/fstate.inc"
#include "cojets/iflghv.inc"
#include "cojets/idrun.inc"
#include "cojets/inmat.inc"
#include "cojets/intype.inc"
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/kdump.inc"
#include "cojets/keybre.inc"
#include "cojets/khadro.inc"
#include "cojets/maxn.inc"
#include "cojets/nevol.inc"
#include "cojets/njsetb.inc"
#include "cojets/nleave.inc"
#include "cojets/nquaz.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/tleave.inc"
#include "cojets/weakon.inc"
#include "cojets/zpar2.inc"
      DATA ZH,WH/2.,3./,DH/4./
      DATA IEVTOL/0/
      DATA ICALL/0/
C
      IF(ICALL.EQ.0) CALL BPREGN
      ICALL=1
700   IFIREF=0
C
C-- INITIALIZATION
C-- EVOLUTION SCALE
      IF(MOPTWZ.EQ.1.AND.WEAKON.EQ.DH) THEN
        QSQ=SXX
      ELSE
        QSQ=BIM2
      ENDIF
      QSQV(1)=QSQ
      ALQ=(LOG(QSQ)-ALLAM2)/ALQZM
      YQSQ=LOG(ALQ)
      XMIN=XMINB
      ALQF=ALQFB
      YF=YFB
      WEIGHT=1.
C-- GENERATE PAIR OF INITIAL PARTONS
      CALL BGPAIR
C-- GENERATE PARTON CASCADE (LLA QCD)
      JBOOK=2
      DO 12 IB=1,2
      NBOOK(IB)=1
      MBOOK(IB)=0
      ISTOP(IB)=0
12    IBACK(IB)=0
      IBEAM=1
      ZERO=0.
      CALL Q2GEN(IFLING(1),ZERO,YIN1,QSQIN1)
      CALL BOOK(IFLING(1),0,XMING(1),YIN1,XING(1),PXING(1),PYING(1)
     +,ZERO,YF)
      IBACK(IBEAM)=0
100   CONTINUE
      IBEAM=1
      CALL EVOL(1,YQSQ)
      MMBOOK=MAX(MMBOOK,MBOOK(1))
      IF(ISTOP(1).EQ.1) GO TO 200
      IBS=1
      MBOOK(2)=0
      IBEAM=2
      CALL Q2GEN(IFLING(2),ZERO,YIN2,QSQIN2)
      CALL BOOK(IFLING(2),0,XMING(2),YIN2,XING(2),PXING(2),PYING(2)
     +,ZERO,YF)
      IBACK(IBEAM)=0
      ISTOP(IBEAM)=0
150   CONTINUE
      CALL EVOL(1,YQSQ)
      MMBOOK=MAX(MMBOOK,MBOOK(2))
      IF(ISTOP(2).EQ.1) GO TO 100
C-- PREPARE TO ENTER HARD PROCESS
      IFLA1=PARACT(1,1,1)
      XM1=PARACT(1,3,1)
      XP1=PARACT(1,5,1)
      PX1=PARACT(1,6,1)
      PY1=PARACT(1,7,1)
      MFLA1=MIN(4,ABS(IFLA1))
      IFLA2=PARACT(1,1,2)
      XM2=PARACT(1,3,2)
      XP2=PARACT(1,5,2)
      PX2=PARACT(1,6,2)
      PY2=PARACT(1,7,2)
C-- CHECK WHETHER RIGHT FLAVOR COMBINATION
      IF(IFLA1.EQ.LGLUS.OR.IFLA2.EQ.LGLUS) GO TO 150
      IF(WEAKON.EQ.ZH) GO TO 301
      IF(WEAKON.EQ.DH) GO TO 302
C-- W BOSON
      IF(IFLA1*IFLA2.GE.0) GO TO 150
      IIFLA=IMATBO(ABS(IFLA1),ABS(IFLA2))
      IF(IIFLA.EQ.0) GO TO 150
      IF(IIFLA.EQ.1) GO TO 310
      IF(CJRN(IIFLA).GT.TG2CAB) GO TO 150
      GO TO 310
C-- Z BOSON
301   IF((IFLA1+IFLA2).NE.0) GO TO 150
      IF(ICHRGQ(ABS(IFLA1)).LT.0) GO TO 310
      IF(MOPTWZ.NE.0) GO TO 310
      IF(CJRN(IFLA1).GT.FZUDS) GO TO 150
      GO TO 310
302   CONTINUE
C-- DRELL-YAN
      IF(MOPTWZ.NE.0) GO TO 301
      IF((IFLA1+IFLA2).NE.0) GO TO 150
      IF(ICHRGQ(ABS(IFLA1)).GT.0) GO TO 310
      IF(CJRN(IFLA1).GT.FZUDS) GO TO 150
310   CONTINUE
C-- CHECK WHETHER SHAT STAYS WITHIN BOSON MASS INTERVAL
      SHAT=(XP1*XP2+XM1*XM2)*S-2.*(PX1*PX2+PY1*PY2)
      IF(MOPTWZ.EQ.0) THEN
      IF(SHAT.LT.BIML2.OR.SHAT.GT.BIMX2) GO TO 150
      IF(WEAKON.EQ.DH) THEN
        IF(CJRN(0.).GT.BIM2/SHAT) GO TO 150
      ENDIF
      ELSE IF(WEAKON.EQ.WH) THEN
        IF(SHAT.LT.BIML2) GO TO 150
        SX=SHAT
        SIG=SW(1)*SX/((SX-SW(2))**2+SW(3))
        IF(IFDC.EQ.18) THEN
          RHOB=SW(4)/SX
          RHOT=SW(5)/SX
          RHOP=RHOT+RHOB
          RHOM2=(RHOT-RHOB)**2
          SIG=SIG*(1.-.5*RHOP-.5*RHOM2)*SQRT(ABS(1.-2.*RHOP+RHOM2))
        ENDIF
        IF(IFDC.LE.0) THEN
          FSTOP=0.
          IF(SX.GT.TPTHR2) THEN
            RHOB=SW(4)/SX
            RHOT=SW(5)/SX
            RHOP=RHOT+RHOB
            RHOM2=(RHOT-RHOB)**2
            FSTOP=(1.-.5*RHOP-.5*RHOM2)*SQRT(ABS(1.-2.*RHOP+RHOM2))
          ENDIF
          SIG=SIG*(9.+3.*FSTOP)
        ENDIF
C
        IF(SIGX*CJRN(0).GT.SIG) GO TO 150
C
        IF(IFDC.LT.0) THEN
          DO 14 LL=1,6
14        CBR1(LL+12)=BRW(LL)
          CBR1(18)=CBR1(18)*FSTOP
          DO 15 LL=2,6
15        CBR1(LL+12)=CBR1(LL+12)+CBR1(LL+11)
          DO 16 LL=1,6
16        CBR1(LL+12)=CBR1(LL+12)/CBR1(18)
        ENDIF
      ELSE
C-- ZH AND DH
        IF(SHAT.LT.BIML2.OR.SHAT.GT.BIMX2) GO TO 150
        LXI=1
        IF(ICHRGQ(ABS(IFLA1)).LT.0) LXI=2
        SX=SHAT
        DS=SX-SZ(1)
        DEN=1./(DS**2+SZ(2))
        IF(IFDC.EQ.12) THEN
          LXF=IFDC
          RHO4=SZ(3)/SX
          SIG=FACTSZ(LXF)*SQRT(ABS(1.-RHO4))*
     *        ((2.+RHO4)*(SZF(1,LXI,LXF)/SX
     *                   +SZF(2,LXI,LXF)*DS*DEN)
     *        +(SZF(3,LXI,LXF)+RHO4*SZF(4,LXI,LXF))*SX*DEN)
        ELSE IF(IFDC.GT.0) THEN
          LXF=IFDC
          SIG=FACTSZ(LXF)*
     *        (2.*(SZF(1,LXI,LXF)/SX
     *            +SZF(2,LXI,LXF)*DS*DEN)
     *        +SZF(3,LXI,LXF)*SX*DEN)
        ELSE
          LXF=13
          SIG=FACTSZ(LXF)*
     *        (2.*(SZF(1,LXI,LXF)/SX
     *            +SZF(2,LXI,LXF)*DS*DEN)
     *        +SZF(3,LXI,LXF)*SX*DEN)
          IF(SX.GT.TPTHR2) THEN
            LXF=12
            RHO4=SZ(3)/SX
            SIG=SIG+
     *        FACTSZ(LXF)*SQRT(ABS(1.-RHO4))*
     *        ((2.+RHO4)*(SZF(1,LXI,LXF)/SX
     *                   +SZF(2,LXI,LXF)*DS*DEN)
     *        +(SZF(3,LXI,LXF)+RHO4*SZF(4,LXI,LXF))*SX*DEN)
          ENDIF
        ENDIF
C
        IF(SIGX*CJRN(0).GT.SIG) GO TO 150
C
        IF(IFDC.LT.0.AND.WEAKON.NE.DH) THEN
          DO 17 LXF=1,12
          IF(LXF.EQ.12) THEN
            CBR1(LXF)=0.
            IF(SX.LE.TPTHR2) GO TO 17
            RHO4=SZ(3)/SX
            CBR1(LXF)=FACTSZ(LXF)*SQRT(ABS(1.-RHO4))*
     *                ((2.+RHO4)*(SZF(1,LXI,LXF)/SX
     *                           +SZF(2,LXI,LXF)*DS*DEN)
     *                +(SZF(3,LXI,LXF)+RHO4*SZF(4,LXI,LXF))*SX*DEN)
          ELSE
            CBR1(LXF)=FACTSZ(LXF)*
     *                (2.*(SZF(1,LXI,LXF)/SX
     *                    +SZF(2,LXI,LXF)*DS*DEN)
     *                +SZF(3,LXI,LXF)*SX*DEN)
          ENDIF
17        CONTINUE
          DO 18 LXF=2,12
18        CBR1(LXF)=CBR1(LXF)+CBR1(LXF-1)
          DO 19 LXF=1,12
19        CBR1(LXF)=CBR1(LXF)/CBR1(12)
        ENDIF
      ENDIF
C
      SHARD=SHAT
      SH=SHAT
      IF(KEYBRE.LT.2)
     *CALL TEVOL(IBS)
      NJSETB=NJSET
      IF(KEYBRE.GE.2) THEN
        NQUA=NQUAZ
        NJSET=NJSETZ
      ENDIF
      NQUAB=NQUA
      IBS=2
      NEVENB=NEVENB+1
      NPART=0
      CALL BOSO
      IF(IFLGHV.EQ.1) GO TO 215
      IF(KMPSCA.NE.1) GO TO 220
      NEVENT=NEVENB
      LLFOR=LFORSL
      LFORSL=0
      IF(KHADRO.EQ.1)
     *CALL HADRON(1,NQUAB,1)
      LFORSL=LLFOR
      IF(WEIGHT.LT.1.E-30) GO TO 215
      CALL TOPDCY
      IF(WEIGHT.LT.1.E-30) GO TO 215
      IF(IFLGHV.EQ.1) GO TO 215
      MNQUA=MAX(MNQUA,NQUA)
      MNJSET=MAX(MNJSET,NJSET)
      IF(NOUNST) CALL STABPH
      CALL FILLH
      IF(INTYPE.EQ.0) THEN
        IF(NEVENT.LE.NDUMP.AND.(MOD(NEVENT,NJUMP).EQ.1.OR.NJUMP.EQ.1))
     *  CALL DUMPEV
      ELSE IF(INTYPE.EQ.1.AND.IEVT.NE.IEVTOL) THEN
        IF(IEVT.LE.NDUMP.AND.(MOD(IEVT,NJUMP).EQ.1.OR.NJUMP.EQ.1))
     *  CALL DUMPEV
        IEVTOL=IEVT
      ENDIF
      IF(INTYPE.EQ.0) THEN
        IF(KMPSCA.EQ.1.AND.NEVENT.GE.NLEAVE) RETURN
      ELSE IF(INTYPE.EQ.1) THEN
        IF(KMPSCA.EQ.1.AND.IEVT.GE.NLEAVE) RETURN
      ENDIF
      GO TO 221
C-- MULTIPLE PARTON PROCESS SET-UP
220   CALL WRITEB
221   CONTINUE
      IFIREF=1
      GO TO 150
215   CONTINUE
      NEVENB=NEVENB-1
      NEVENT=NEVENB
      WEIGHT=1.
      GO TO 150
200   CONTINUE
C
      IF(IFIREF.EQ.0) GO TO 700
C
      RETURN
      END
