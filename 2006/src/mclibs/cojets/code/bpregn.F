*
* $Id: bpregn.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: bpregn.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BPREGN
C     *****************
C-- W/Z (OR DRELL-YAN PAIR) PRODUCTION -- INITIALIZATION AND X-TABLES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/boends.inc"
#include "cojets/bopar.inc"
#include "cojets/botqrk.inc"
#include "cojets/cfun1.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/decpar.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/evtype.inc"
#include "cojets/itapes.inc"
#include "cojets/keybre.inc"
#include "cojets/nevol.inc"
#include "cojets/nflav.inc"
#include "cojets/parq.inc"
#include "cojets/qcds.inc"
#include "cojets/stable.inc"
#include "cojets/transl.inc"
#include "cojets/weakbo.inc"
#include "cojets/weakon.inc"
#include "cojets/zpar2.inc"
#include "cojets/zwpar.inc"
      EXTERNAL FUN1,FUN3
      DATA ZH,WH/2.,3./,DH/4./
      DATA ICALL/0/
      IF(ICALL.GT.0) RETURN
      ICALL=1
C
C
C-- INITIALIZATION
      CALL SBLOCK
C-- CALL WEAKIN/DREYIN TO INITIALIZE EVENT SPECIFICS
      IF(IEVTYP.EQ.2) CALL WEAKIN(ZH,KOPTWZ,IDECBO,LWIDTH,LEPRAD)
      IF(IEVTYP.EQ.3) CALL WEAKIN(WH,KOPTWZ,IDECBO,LWIDTH,LEPRAD)
      IF(IEVTYP.EQ.4) CALL DREYIN(KOPTWZ,IDECBO,LEPRAD)
      S=ECM**2
      NFLAVS=NFLAV
      LGLUS=7
      IVAL=100
      PI=4.*ATAN(1.)
      JBOOK=1
      NEVENT=0
      NEVOL=0
      NEVOLB=0
      NGEX=0
      ALLAM2=LOG(ALAMB**2)
      ALQZM=LOG((QZEV/ALAMB)**2)
      ZMCUT=1.
      ZLW=ZMCUT/ECM
      ZUP=1.-ZLW
C
C-- BOSON PARAMETERS
      WEAKON=WEAKBO
      MOPTWZ=KOPTWZ
      IFDC=IDECWK
      IF(WEAKON.EQ.ZH) GO TO 20
      IF(WEAKON.EQ.DH) GO TO 40
C-- W
      C2CAB=COSCAB**2
      TG2CAB=(1.-C2CAB)/C2CAB
      IF(MOPTWZ.EQ.0) THEN
      BIM=PMAS(3)
      BIM2=BIM**2
      BIML2=(BIM*.95)**2
      BIMX2=(BIM*1.05)**2
      FXSCT=(GF*PI*SQRT(2.))*C2CAB
     1      /3.
     2      *BIM2/(BIMX2-BIML2)
     *      *(1.-DELTAR)**2
     3      *(.19733)**2*1.E1
      WIDTH=WGAM
      IF(IWIDTH.EQ.0) WIDTH=0.
      ELSE
        BIM=PMAS(3)
        BIM2=BIM**2
        TPTHR=QMAS(6)+QMAS(5)+1.
        TPTHR2=TPTHR**2
        IF(WZTHRS.LT.0.) WZTHRS=PMAS(3)-4.*WGAM
        IF(IFDC.EQ.18.AND.WZTHRS.LT.TPTHR)
     *    WZTHRS=TPTHR
        BIML2=WZTHRS**2
        SW(1)=PI*ALFQED**2/(12.*S2THW**2)/3.*C2CAB*(.19733)**2*1.E1
        IF(IFDC.GE.16) SW(1)=SW(1)*3.
        SW(2)=BIM2
        SW(3)=BIM2*WGAM**2
        SW(4)=QMAS(5)**2
        SW(5)=QMAS(6)**2
C-- W SIGX
        DSX=BIM*WGAM
        SX=BIML2
        SIGOLD=0.
        DO 16 NITER=1,5
17      CONTINUE
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
        IF(SIG.LE.SIGOLD) THEN
          SIGX=SIGOLD
          SIGOLD=0.
          SX=SX-2.*DSX
          IF(SX.LT.BIML2) SX=BIML2
          DSX=DSX*.1
          GO TO 16
        ELSE
          SIGOLD=SIG
          SX=SX+DSX
          GO TO 17
        ENDIF
16      CONTINUE
        FXSCT=SIGX
        WIDTH=WGAM
        IF(IFDC.LT.0) THEN
          DO 22 LL=1,3
          BRW(LL)=1.
          BRW(LL+3)=3.
22        CONTINUE
        ENDIF
      ENDIF
      GO TO 30
C-- Z
20    CONTINUE
      XW=S2THW
      IF(MOPTWZ.EQ.0) THEN
      FU=.25-2./3.*XW+8./9.*XW**2
      FDS=.25-XW/3.+2./9.*XW**2
      FZUDS=FU/FDS
      BIM=PMAS(2)
      BIM2=BIM**2
      BIML2=(BIM*.95)**2
      BIMX2=(BIM*1.05)**2
      FXSCT=(GF*PI*SQRT(2.)*2.)*FDS
     1      /3.
     2      *BIM2/(BIMX2-BIML2)
     *      *(1.-DELTAR)**2
     3      *(.19733)**2*1.E1
      WIDTH=ZGAM
      IF(IWIDTH.EQ.0) WIDTH=0.
      ELSE
        BIM=PMAS(2)
        BIM2=BIM**2
        IF(WEAKON.EQ.ZH) THEN
          TPTHR=2.*QMAS(6)+1.
          TPTHR2=TPTHR**2
          IF(WZTHRS.LT.0.) WZTHRS=PMAS(2)-4.*ZGAM
          IF(IFDC.EQ.12.AND.WZTHRS.LT.TPTHR)
     *      WZTHRS=TPTHR
          BIML2=WZTHRS**2
          BIMX2=ECM**2
        ELSE
          BIML2=BOMSMN**2
          BIMX2=BOMSMX**2
          TPTHR2=ECM**2
        ENDIF
        SZ(1)=BIM2
        SZ(2)=BIM2*ZGAM**2
        SZ(3)=4.*QMAS(6)**2
        DO 24 LL2=1,2
        DO 24 LL=1,8
24      SZF(LL,LL2,13)=0.
C
        NXF=12
        IF(WEAKON.EQ.DH) NXF=3
        DO 23 LXF=1,NXF
        FACTSZ(LXF)=PI*ALFQED**2*2./9.*(.19733)**2*1.E1
        IF(LXF.GE.7) FACTSZ(LXF)=FACTSZ(LXF)*3.
        IF(LXF.LE.3) THEN
          QF=1.
          VF=AL(1)
          AF=BL(1)
        ELSE IF(LXF.LE.6) THEN
          QF=0.
          VF=AL(2)
          AF=BL(2)
        ELSE IF(CHARGE(LXF-6).GT.0.) THEN
          QF=CHARGE(LXF-6)
          VF=AQ(1)
          AF=BQ(1)
        ELSE
          QF=CHARGE(LXF-6)
          VF=AQ(2)
          AF=BQ(2)
        ENDIF
        FXW=1./SQRT(ABS(XW*(1.-XW)))
        VF=VF*FXW
        AF=AF*FXW
        DO 21 LI=1,2
        QI=CHARGE(LI)
        VI=AQ(LI)*FXW
        AI=BQ(LI)*FXW
        SZF(1,LI,LXF)=QI**2*QF**2
        SZF(2,LI,LXF)=-2.*QI*VI*QF*VF
        SZF(3,LI,LXF)=2.*(VI**2+AI**2)*(VF**2+AF**2)
        SZF(4,LI,LXF)=(VI**2+AI**2)*(VF**2-2.*AF**2)
        SZF(5,LI,LXF)=.5*SZF(3,LI,LXF)
        SZF(6,LI,LXF)=(VI**2+AI**2)*(VF**2-AF**2)
        SZF(7,LI,LXF)=-4.*QI*AI*QF*AF
        SZF(8,LI,LXF)=8.*VI*AI*VF*AF
        FCOLRS=1.
        IF(LXF.GE.7) FCOLRS=3.
        IF(LXF.NE.12) THEN
          DO 25 LL=1,8
25        SZF(LL,LI,13)=SZF(LL,LI,13)+FCOLRS*SZF(LL,LI,LXF)
        ENDIF
21      CONTINUE
23      CONTINUE
        FACTSZ(13)=FACTSZ(1)
C-- Z SIGX
        SIGX=0.
        DO 28 LXI=1,2
        DSX=BIM*ZGAM
        SX=BIML2
        SIGOLD=0.
        SXSTOP=BIMX2
        DO 14 NITER=1,5
15      CONTINUE
        DS=SX-SZ(1)
        DEN=1./(DS**2+SZ(2))
        IF(IFDC.GT.0) THEN
          LXF=IFDC
          RHO4=0.
          IF(IFDC.EQ.12) RHO4=SZ(3)/SX
          SIG=FACTSZ(LXF)*SQRT(ABS(1.-RHO4))*
     *          ((2.+RHO4)*(SZF(1,LXI,LXF)/SX
     *                     +SZF(2,LXI,LXF)*DS*DEN)
     *          +(SZF(3,LXI,LXF)+RHO4*SZF(4,LXI,LXF))*SX*DEN)
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
        IF(WEAKON.EQ.ZH) THEN
          IF(SIG.LE.SIGOLD) THEN
            SIG=SIGOLD
            SIGOLD=0.
            SX=SX-2.*DSX
            IF(SX.LT.BIML2) SX=BIML2
            DSX=DSX*.1
            GO TO 14
          ELSE
            SIGOLD=SIG
            SX=SX+DSX
            GO TO 15
          ENDIF
        ELSE
          IF(SIG.GT.SIGOLD) THEN
            SIGOLD=SIG
            SXX=SX
          ENDIF
          SX=SX+DSX
          IF(SX.LT.SXSTOP) GO TO 15
          SIG=SIGOLD
          DSX=DSX*.1
          SX=SXX-10.*DSX
          IF(SX.LT.BIML2) SX=BIML2
          SXSTOP=SX+10.*DSX
          IF(SXSTOP.GT.BIMX2) SXSTOP=BIMX2
          GO TO 14
        ENDIF
14      CONTINUE
        IF(SIG.GT.SIGX) SIGX=SIG
28      CONTINUE
        FXSCT=SIGX
        WIDTH=ZGAM
      ENDIF
      GO TO 30
C-- DRELL-YAN
40    CONTINUE
      IF(MOPTWZ.EQ.1) GO TO 20
      BIML2=BOMSMN**2
      BIMX2=BOMSMX**2
      BIM2=BIML2
      WIDTH=0.
      FZUDS=.25
      FXSCT=(4./3.*PI*ALFQED**2)/BIM2**2*4./9.
     1      /3.
     2      *BIM2
     3      *(.19733)**2*1.E1
      IF(IFDC.LT.0) FXSCT=3.*FXSCT
30    CONTINUE
C
C-- COMPLETE INITIALIZATION
      XMIN=(BIML2-5.*PT2INT)/S
      IF(MOPTWZ.EQ.1.AND.WEAKON.EQ.DH) THEN
        ALQF=(LOG(SXX)-ALLAM2)/ALQZM
      ELSE
        ALQF=(LOG(BIM2)-ALLAM2)/ALQZM
      ENDIF
      YF=LOG(ALQF)
      XMINB=XMIN
      ALQFB=ALQF
      YFB=YF
      IFCODE(1)=1
      IFCODE(2)=-1
      IFCODE(3)=2
      IFCODE(4)=-2
      IFCODE(5)=3
      CALL INQCDS
C
      ALAMBT=ALAMB
      NFLATR=NFLAV
      QSQMXT=ECM**2
      CALL INIQCD
C
C-- PREPARE X-TABLES
      NBIN=127
      DBIN=1./FLOAT(NBIN-1)
      HDBIN=DBIN/2.
      XVMIN=DBIN
      EPSI=.001
C-- QUARKS
      XV(1)=0.
      DO 8 J=1,5
      TABQRK(1,J)=0.
      DO 8 IX=2,NBIN
      X1=DBIN*FLOAT(IX-2)
      X2=X1+DBIN
      XV(IX)=X1+HDBIN
      IF(X2.LE.XMIN) GO TO 9
      X1=MAX(X1,XMIN)
      IFUN=J
      TABQRK(IX,J)=ASIMP(LOG(X1),LOG(X2),EPSI,M,2,FUN1)
      GO TO 8
9     TABQRK(IX,J)=0.
      XVMIN=X2+DBIN
8     CONTINUE
      DO 1 IX=1,NBIN
      TABQRK(IX,6)=0.
      DO 1 J=1,5
      TABQRK(IX,6)=TABQRK(IX,6)+TABQRK(IX,J)
1     CONTINUE
      WGQ=0.
      DO 10 IX=1,NBIN
10    WGQ=WGQ+TABQRK(IX,6)
C-- GLUONS
      TABQRK(1,7)=0.
      DO 11 IX=2,NBIN
      X1=DBIN*FLOAT(IX-2)
      X2=X1+DBIN
      IF(X2.LE.XMIN) GO TO 12
      X1=MAX(X1,XMIN)
      TABQRK(IX,7)=ASIMP(LOG(X1),LOG(X2),EPSI,M,2,FUN3)
      GO TO 11
12    TABQRK(IX,7)=0.
11    CONTINUE
      WGG=0.
      DO 13 IX=1,NBIN
13    WGG=WGG+TABQRK(IX,7)
C
C-- COMPLETE X-SECTION FACTOR
      FINT=WGQ+WGG
      FXSCT=FXSCT*FINT**2
C
      RETURN
      END
