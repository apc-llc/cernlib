*
* $Id: aradig.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: aradig.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aradig.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARADIG(ID)
 
C...Ariadne RADiater Initial state G->qq
 
C...Perform an initial-state g->qqbar splitting.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARSTRF/ KFSAVE(2),XSAVE(2),XQ2SAV(2),XPQSAV(2,-6:6)
      SAVE /ARSTRF/
      COMMON /ARLIST/ B1SAVE(2),B3SAVE(2),IPTOT(MAXPAR),NPTOT,
     $     IPSTQ(MAXPAR),NPSTQ,IPREM(MAXPAR),NPREM,IRDIR(2),
     $     YIQQ(2),PT2IQQ(2),PT2SAV(2),IRASAV(2),A1SAVE(2),A3SAVE(2)
 
      SAVE /ARLIST/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      IF (MHAR(120).NE.0) THEN
        CALL ARADG2(ID)
        RETURN
      ENDIF
 
      IF (ABS(MSTA(33)).EQ.1.AND.MSTA(1).EQ.3.AND.IO.EQ.1) THEN
        LST(24)=3
        QEXDIS=.TRUE.
      ELSE
        QEXDIS=.FALSE.
      ENDIF
      IRP=IRAD(ID)-10000
      IT=IRP+5-MAXPAR
      IR=INQ(IRP)
      NPREM=2
      IPREM(1)=IRP
      IPREM(2)=IR
      IDIR=IRDIR(IT)
      DIR=IDIR
      KQ=IDO(IRP)
      RMQ2=ULMASS(KQ)
      PM=P(IT,4)+IDIR*P(IT,3)
      DMT2Q=PT2IQQ(IT)
      DPT2Q=DMT2Q-RMQ2**2
      YQ=AEX1(ID)
      PHIQ=BX1(ID)
 
      IF (MHAR(103).GT.0) THEN
        NPSTQ=0
        DO 110 I=1,NPTOT
          IF (INO(IPTOT(I)).NE.0) THEN
            NPSTQ=NPSTQ+1
            IPSTQ(NPSTQ)=IPTOT(I)
          ENDIF
 110    CONTINUE
        CALL ARPADD(-IDI(IRP),NPSTQ,IPSTQ)
      ELSE
        NPSTQ=1
        IPSTQ(1)=IDI(IRP)
      ENDIF
 
      CALL ARROBO(0.0,-PHIQ,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
      CALL ARSUME(0,DXT,DYT,DZT,DET,DMT,NPREM,IPREM)
        CALL ARSUME(1,DXT,DYT,DZT,DET,DMT,NPSTQ,IPSTQ)
      B0P=DEQ-IDIR*DZQ
      B0M=DEQ+IDIR*DZQ
      BRP=DER-IDIR*DZR
      BRM=DER+IDIR*DZR
 
      XX=1.0-BRM/PM
      IF (QEXDIS) XX=X
 
      IF (MHAR(119).GT.0) THEN
        Z=YQ
        DRM=(1.0-XX/Z)*PM
        DR=BRM
        CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $       DIR*(DRM**2-DR**2)/(DRM**2+DR**2),NPREM,IPREM)
        BPH=B0P+BRP-BRP*BRM/DRM
        BMH=B0M+BRM-DRM
 
        DXQ2=SQRT(DPT2Q)
        RMTQ=SQRT(DMT2Q)
        RMTS=SQRT(DMQ**2+DYQ**2+(DXQ-DXQ2)**2)
        STOT=BPH*BMH
        DZS=ARZCMS(STOT,RMTS,RMTQ)
        DZSP=SQRT(DZS**2+(DXQ-DXQ2)**2-DXQ**2)
        DPSP=DZSP+SQRT(DZSP**2+DXQ**2+DYQ**2+DMQ**2)
        DP=B0P
        CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $       DIR*(DP**2-DPSP**2)/(DP**2+DPSP**2),NPSTQ,IPSTQ)
        DXZQ=-DIR*SQRT(DXQ**2+DZSP**2)
        IF (ABS(DXZQ).LE.ABS(DXQ-DXQ2)) THEN
          CALL ARERRM('ARADIG',9,0)
          GOTO 900
        ENDIF
        CALL ARROBO(REAL(ASIN((DXQ-DXQ2)/DXZQ)-ASIN(DXQ/DXZQ)),
     $     0.0,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
 
        IPART=IPART+1
        IQ2=IPART
        IFL(IQ2)=-KQ
        IF (MSTA(30).LT.2.OR.MSTA(30).EQ.3) THEN
          QEX(IQ2)=.FALSE.
          XPMU(IQ2)=0.0
          XPA(IQ2)=0.0
        ELSE
          QEX(IQ2)=.TRUE.
          IF (PARA(14).GE.0) THEN
            XPMU(IQ2)=SQRT(XQ2SAV(IT))*PARA(14)
          ELSE
            XPMU(IQ2)=ABS(PARA(14))
          ENDIF
          XPA(IQ2)=PARA(15)
        ENDIF
        QEX(IQ2)=.FALSE.
        QQ(IQ2)=.TRUE.
        INO(IQ2)=IO
        INQ(IQ2)=0
        BP(IQ2,5)=RMQ2
        BP(IQ2,1)=DXQ2
        BP(IQ2,2)=0.0
        BP(IQ2,3)=DIR*DZS
        BP(IQ2,4)=SQRT(RMQ2**2+DZS**2+DPT2Q)
        NPSTQ=NPSTQ+1
        IPSTQ(NPSTQ)=IQ2
 
        CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
        DM=DEQ-IDIR*DZQ
        DMH=BMH
        CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $       DIR*(DMH**2-DM**2)/(DMH**2+DM**2),NPSTQ,IPSTQ)
 
        CALL ARSUME(0,DXU,DYU,DZU,DEU,DMU,NPREM,IPREM)
        CALL ARSUME(1,DXU,DYU,DZU,DEU,DMU,NPSTQ,IPSTQ)
 
        GOTO 100
 
      ENDIF
 
      BXQ=SQRT(DPT2Q)
      BYQ=0.0
      BZQ=SQRT(DMT2Q)*SINH(YQ)
      BEQ=SQRT(DMT2Q)*COSH(YQ)
      BQP=BEQ-IDIR*BZQ
      BQM=BEQ+IDIR*BZQ
 
      BM0D2=DMQ**2+(DXQ-BXQ)**2+(DYQ-BYQ)**2
      BRQP=B0P+BRP-BQP
      BRQM=B0M+BRM-BQM
 
      BA=(BRQP*BRQM+BRP*BRM-BM0D2)/(2.0*BRQM*BRP)
      BB=BRM*BRQP/(BRP*BRQM)
 
      IF (BA**2.LT.BB.OR.BA.LE.0.0.OR.BRQP.LE.0.0.OR.BRQM.LE.0.0) THEN
        CALL ARERRM('ARADIG',9,0)
        GOTO 900
      ENDIF
 
      DAR=BA-SQRT(BA**2-BB)
 
      IF (DAR.LE.1.0) CALL ARERRM('ARADIG',9,0)
 
      DXZQ=SIGN(SQRT(DXQ**2+DZQ**2),DZQ)
      IF (ABS(DXZQ).LE.ABS(DXQ-BXQ)) THEN
        CALL ARERRM('ARADIG',9,0)
        GOTO 900
      ENDIF
 
C...Boost remnant system to correct rapidity
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     -DIR*(DAR**2-1.0D0)/(DAR**2+1.0D0),NPREM,IPREM)
 
C...Rotate struck system to right pt
      CALL ARROBO(REAL(ASIN((DXQ-BXQ)/DXZQ)-ASIN(DXQ/DXZQ)),
     $     0.0,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
 
C...Boost struck system to right rapidity
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
      DPP2=(BRP*(1.0-DAR)+B0P-BQP)**2
      DPP02=(DZQ-IDIR*DEQ)**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,-DIR*(DPP2-DPP02)/(DPP2+DPP02),
     $     NPSTQ,IPSTQ)
 
 
C...Insert new quark
      IPART=IPART+1
      IQ2=IPART
      IFL(IQ2)=-KQ
      IF (MSTA(30).LT.2.OR.MSTA(30).EQ.3) THEN
        QEX(IQ2)=.FALSE.
        XPMU(IQ2)=0.0
        XPA(IQ2)=0.0
      ELSE
        QEX(IQ2)=.TRUE.
        IF (PARA(14).GE.0) THEN
          XPMU(IQ2)=SQRT(XQ2SAV(IT))*PARA(14)
        ELSE
          XPMU(IQ2)=ABS(PARA(14))
        ENDIF
        XPA(IQ2)=PARA(15)
      ENDIF
CERROR      QEX(IQ2)=.FALSE.
      QQ(IQ2)=.TRUE.
      INO(IQ2)=IO
      INQ(IQ2)=0
      BP(IQ2,5)=RMQ2
      BP(IQ2,4)=BEQ
      BP(IQ2,3)=BZQ
      BP(IQ2,2)=BYQ
      BP(IQ2,1)=BXQ
      CALL AROBO1(0.0,PHIQ,0.0D0,0.0D0,0.0D0,IQ2)
 
C...Insert new remnant
 100  IPART=IPART+1
      IR=IPART
      IFL(IR)=INO(IRP)
      QEX(IR)=QEX(IRP)
      QQ(IR)=.TRUE.
      INO(IR)=0
      INQ(IR)=0
      XPMU(IR)=XPMU(IRP)
      XPA(IR)=XPA(IRP)
      BP(IR,1)=BP(IRP,1)
      BP(IR,2)=BP(IRP,2)
      BP(IR,3)=BP(IRP,3)
      BP(IR,4)=BP(IRP,4)
      BP(IR,5)=BP(IRP,5)
      QQ(IRP)=.FALSE.
 
C...Fix new string and dipole
      IDIPS=IDIPS+1
      ISTRS=ISTRS+1
      CALL ARCRDI(IDIPS,IQ2,IR,ISTRS,.FALSE.)
      IDI(IQ2)=0
      IDO(IR)=0
      IPF(ISTRS)=IQ2
      IPL(ISTRS)=IR
      IFLOW(ISTRS)=SIGN(1,-KQ)
      CALL ARCOLI(IDIPS,ID)
 
C...Reset all dipole flags
      DO 200 IDD=1,IDIPS
        QDONE(IDD)=.FALSE.
 200  CONTINUE
 
 900  CALL ARROBO(0.0,PHIQ,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
 
      RETURN
 
C**** END OF ARADIG ****************************************************
      END
