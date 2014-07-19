*
* $Id: arging.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arging.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arging.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARGING(ID,IRP)
 
C...ARiadne Generate INitial state G->QQ
 
C...Generate kinematical variables describing an initial-state g->qqbar
C...splitting.
 
 
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
      COMMON /ARLIST/ B1SAVE(2),B3SAVE(2),IPTOT(MAXPAR),NPTOT,
     $     IPSTQ(MAXPAR),NPSTQ,IPREM(MAXPAR),NPREM,IRDIR(2),
     $     YIQQ(2),PT2IQQ(2),PT2SAV(2),IRASAV(2),A1SAVE(2),A3SAVE(2)
 
      SAVE /ARLIST/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
 
      INTEGER NTT,NTE1,NTE2
      DATA NTT/0/,NTE1/0/,NTE2/0/
 
 
      IF (MHAR(120).NE.0) THEN
        CALL ARGIG2(ID,IRP)
        RETURN
      ENDIF
 
      IF (MHAR(102).LT.0) RETURN
 
      QEXDIS=(MSTA(1).EQ.3.AND.IO.EQ.0)
 
      IR=INQ(IRP)
      IT=IRP+5-MAXPAR
 
      IF (IRAD(ID).EQ.10000+IRP) THEN
        PT2IN(ID)=PT2SAV(IT)
        IRAD(ID)=IRASAV(IT)
        AEX1(ID)=A1SAVE(IT)
        AEX3(ID)=A3SAVE(IT)
        BX1(ID)=B1SAVE(IT)
        BX3(ID)=B3SAVE(IT)
      ENDIF
 
      NPREM=2
      IPREM(1)=IRP
      IPREM(2)=IR
      IDIR=IRDIR(IT)
      KQ=IDO(IRP)
      KF=K(IT,2)
      RMQ=ULMASS(KQ)
      PM=P(IT,4)+IDIR*P(IT,3)
      PT2CUT=MAX(PARA(3)**2+RMQ**2,PT2IN(ID))/PHAR(103)
 
      IF (NPTOT.EQ.0) THEN
        DO 10 I=1,IPART
          IPTOT(I)=I
 10     CONTINUE
        NPTOT=IPART
      ENDIF
 
      IF (MHAR(103).GT.0) THEN
        NPSTQ=0
        DO 20 I=1,NPTOT
          IF (INO(IPTOT(I)).NE.0) THEN
            NPSTQ=NPSTQ+1
            IPSTQ(NPSTQ)=IPTOT(I)
          ENDIF
 20     CONTINUE
        CALL ARPADD(-IDI(IRP),NPSTQ,IPSTQ)
      ELSE
        NPSTQ=1
        IPSTQ(1)=IDI(IRP)
      ENDIF
 
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
      B0P=DEQ-IDIR*DZQ
      B0M=DEQ+IDIR*DZQ
      BRP=DER-IDIR*DZR
      BRM=DER+IDIR*DZR
 
      XX=1.0-BRM/PM
      IF (QEXDIS) XX=X
 
      PT2MX=MIN(REAL((SQRT(DXQ**2+DYQ**2+DZQ**2)-
     $     SQRT(DXQ**2+DYQ**2))**2),PT2LST)
      IF (MHAR(119).GT.0) THEN
        RMTQ=SQRT(B0P*B0M)
        STOT=(DEQ+DER)**2-(DZQ+DZR)**2-(DYQ+DYR)**2-(DXQ+DXR)**2
        PT2MX=MIN(ARZCMS(STOT,RMTQ,RMQ)**2,PT2LST)
      ENDIF
      IF (PT2MX.LE.PT2CUT) GOTO 900
      IF (QEXDIS) THEN
        SQ2MAX=0.5*(XQ2+PT2MX)
        SQ2MIN=PT2CUT/
     $       ((0.5+SQRT(MAX(0.25-PT2CUT*X/(XQ2*(1.0-X)),0.0)))*(1.0-X))
      ELSE
        SQ2MAX=PT2MX+XX*B0P*PM
        SQ2MIN=PT2CUT/(1.0-XX)
      ENDIF
 
      XNUMFL=MAX(ARNOFL(SQRT(SQ2MAX),MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)
      IF (MHAR(118).EQ.0) THEN
        STRA0=ARSTRA(KF,KQ,XX,1.0,SQ2MIN)
        DO 30 IQ2=1,20
          SQ2=EXP(LOG(SQ2MIN)+REAL(IQ2)*(LOG(SQ2MAX)-LOG(SQ2MIN))/20.0)
          STRA0=MAX(STRA0,ARSTRA(KF,KQ,XX,1.0,SQ2))
 30     CONTINUE
      ELSE
        STRA0=ARSTRA(KF,KQ,XX,1.0,SQ2MAX)*REAL(MHAR(118))
      ENDIF
      IF (STRA0.LE.0.0) THEN
        GOTO 900
      ENDIF
 
      C=PHAR(104)*ALPHA0*STRA0/PARU(1)
      ZINT=1.0-X
      CN=1.0/(C*ZINT)
      XLAM2=(PARA(1)**2)/PHAR(103)
      IF (QEXDIS) THEN
        CY=(1.0-XY)/(1.0+(1.0-XY)**2)
        CQ=0.125+0.25*CY
        C=PHAR(104)*0.25*ALPHA0*STRA0*CQ/PARU(1)
        THEMAX=PT2MX
        YINT=4.0*LOG(SQRT(PT2MX/PT2CUT)+SQRT(PT2MX/PT2CUT-1.0))
        CN=1.0/(YINT*C)
      ENDIF
 
 100  IF (PT2MX.LE.PT2CUT) GOTO 900
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.
     $     LOG(LOG(PT2CUT/XLAM2)/LOG(PT2MX/XLAM2))) GOTO 900
      PT2MX=XLAM2*(PT2MX/XLAM2)**(ARG**CN)
 
      IF (QEXDIS) THEN
        YMAX=2.0*LOG(SQRT(THEMAX/PT2MX)+SQRT(THEMAX/PT2MX-1.0))
        Y=(RLU(IDUM)*2.0-1.0)*YMAX
        ZQ=1.0/(1.0+EXP(-Y))
        IF (MHAR(102).EQ.2) THEN
          Z=XQ2*ZQ*(1.0-ZQ)/(PT2MX+XQ2*ZQ*(1.0-ZQ))
        ELSE
          Z=ZQ*(1.0-ZQ)*XQ2/PT2MX
        ENDIF
        IF (Z.LE.X.OR.Z.GE.1.0) GOTO 100
        SQ2=PT2MX/
     $     ((0.5+SQRT(MAX(0.25-PT2MX*Z/(XQ2*(1.0-Z)),0.0)))*(1.0-Z))
        W=2.0*YMAX/YINT
        W=W*(Z*(1.0-Z)*(Z**2+(1.0-Z)**2)*(ZQ**2+(1.0-ZQ)**2)+
     $       16.0*((Z*(1.0-Z))**2)*ZQ*(1.0-ZQ)*CY)/CQ
        IF (MSTA(19).EQ.2) THEN
          W=W*MIN(1.0,LOG(PT2MX/XLAM2)/LOG(PARA(21)*XQ2/XLAM2))
          SQ2=MAX(SQ2,XQ2)
        ENDIF
      ELSE
        Z=XX+RLU(0)*(1.0-XX)
        W=(Z**2+(1.0-Z)**2)*0.25
        IF (MHAR(119).EQ.0.AND.
     $       Z.GE.1.0/(1.0+PT2MX/(XX*B0P*PM))) GOTO 100
        SQ2=PT2MX/(1.0-Z)
      ENDIF
 
      IF (MHAR(113).EQ.1) THEN
        STRA=ARSTRA(KF,KQ,XX,Z,SQ2)
        IF (MHAR(118).EQ.0.AND.STRA.LE.0.0) GOTO 100
        IF (STRA.LT.0.0) THEN
          GOTO 100
        ENDIF
        W=W*STRA/STRA0
      ELSE
        BETA=PARA(25)
        IF (MSTA(25).EQ.0) BETA=0.0
        PTIN=SQRT(PHAR(103)*PT2MX)
        IF (MHAR(113).EQ.2) PTIN=2.0*PTIN
        XMU=PARA(11)
        ALPHA=PARA(10)
        IF (PARA(10).GT.0.0) THEN
          XMU=PARA(11)
          ALPHA=PARA(10)
        ELSEIF (PTIN.GE.ABS(PARA(10))) THEN
          XMU=SQRT(ABS(PARA(10)*PARA(11)))
          ALPHA=2.0
        ELSE
          XMU=PARA(11)
          ALPHA=1.0
        ENDIF
        IF (XX/Z.GT.((1.0/RLU(IDUM)-1.0)**BETA)*(XMU/PTIN)**ALPHA)
     $       GOTO 100
      ENDIF
 
      IF (MHAR(118).EQ.0.AND.W.GT.1.0) THEN
        CALL ARERRM('ARGING',22,0)
        GOTO 900
      ENDIF
 
      IF (W.LT.RLU(IDUM)) GOTO 100
 
      IF (MHAR(113).EQ.-1) THEN
        IF (PT2MX.LT.Z*(1.0-X)*XQ2) GOTO 100
        IF (PT2MX.LT.(1.0-Z)*(1.0-X)*XQ2) GOTO 100
      ENDIF
 
      IF (QEXDIS) THEN
        YQ=-IDIR*0.5*LOG(ZQ*(1.0-X)/((1.0-ZQ)*(X/Z-X)))
        XA=0.125*(1.0+(1.0-XY)**2)*(Z**2+(1.0-Z)**2)*
     $       (ZQ**2+(1.0-ZQ)**2)/(ZQ*(1.0-ZQ))+2.0*(1.0-XY)*Z*(1.0-Z)
        XB=0.5*XY*SQRT((1.0-XY)*Z*(1.0-Z)/(ZQ*(1.0-ZQ)))*
     $       (1.0-2.0/XY)*(1.0-2.0*ZQ)*(1.0-2.0*Z)
        XC=(1.0-XY)*Z*(1.0-Z)
        ABC=ABS(XA)+ABS(XB)+ABS(XC)
 200    PHI=PARU(2)*RLU(IDUM)
        IF (XA+XB*COS(PHI)+XC*COS(2.0*PHI).LT.RLU(IDUM)*ABC) GOTO 200
      ELSE
        YQ=-IDIR*0.5*LOG(PT2MX*(Z/((1.0-Z)*XX*PM))**2)
        PHI=PARU(2)*RLU(IDUM)
      ENDIF
 
      IF (MHAR(119).GT.0) THEN
 
        YQ=Z
        BM=(1.0-XX/Z)*PM
        IF (BM.LT.DMR) GOTO 100
        BPH=B0P+BRP-BRP*BRM/BM
        BMH=B0M+BRM-BM
 
        DPT2Q=PT2MX-RMQ**2
        RMTQ=SQRT(PT2MX)
        DXS=DXQ-SQRT(DPT2Q)*COS(PHI)
        DYS=DYQ-SQRT(DPT2Q)*SIN(PHI)
        RMTS=SQRT(DMQ**2+DXS**2+DYS**2)
        STOT=BPH*BMH
        DZS=ARZCMS(STOT,RMTS,RMTQ)
        IF (DZS.LT.0.0) GOTO 100
        IF (DZS**2+DYS**2+DXS**2.LE.DYQ**2+DXQ**2) GOTO 100
 
      ELSE
 
 
        DPT2Q=PT2MX-RMQ**2
        DMT2Q=PT2MX
 
        BXQ=SQRT(DPT2Q)*COS(PHI)
        BYQ=SQRT(DPT2Q)*SIN(PHI)
        BZQ=SQRT(DMT2Q)*SINH(YQ)
        BEQ=SQRT(DMT2Q)*COSH(YQ)
        BQP=BEQ-IDIR*BZQ
        BQM=BEQ+IDIR*BZQ
 
        BM0D2=DMQ**2+(DXQ-BXQ)**2+(DYQ-BYQ)**2
        BRQP=B0P+BRP-BQP
        BRQM=B0M+BRM-BQM
 
        BA=(BRQP*BRQM+BRP*BRM-BM0D2)/(2.0*BRQM*BRP)
        BB=BRM*BRQP/(BRP*BRQM)
 
        IF (BA**2.LT.BB.OR.BA.LE.0.0.OR.BRQP.LE.0.0.OR.BRQM.LE.0.0)
     $       GOTO 100
 
        DAR=BA-SQRT(BA**2-BB)
 
        IF (DAR.LE.1.0) GOTO 100
 
        DQ=SQRT(DXQ**2+DYQ**2+DZQ**2)
        IF (DQ.LE.SQRT((DXQ-BXQ)**2+(DYQ-BYQ)**2)) GOTO 100
 
      ENDIF
 
      NTT=NTT+1
      IF (W.GT.1.0) THEN
        NTE1=NTE1+1
        IF (MOD(NTE1,10).EQ.0) WRITE(0,*) REAL(NTE1)/REAL(NTT)
      ENDIF
      IF (STRA.GT.STRA0) THEN
        NTE2=NTE2+1
        IF (MOD(NTE2,10).EQ.0) WRITE(0,*) REAL(NTE2)/REAL(NTT)
      ENDIF
 
      IF (PT2MX*PHAR(103).GT.PT2IN(ID)) THEN
        PT2SAV(IT)=PT2IN(ID)
        IRASAV(IT)=IRAD(ID)
        A1SAVE(IT)=AEX1(ID)
        A3SAVE(IT)=AEX3(ID)
        B1SAVE(IT)=BX1(ID)
        B3SAVE(IT)=BX3(ID)
        PT2GG(IRP)=PT2MX
        PT2IQQ(IT)=PT2MX
        PT2IN(ID)=PT2MX*PHAR(103)
        IRAD(ID)=10000+IRP
        AEX1(ID)=YQ
        AEX3(ID)=YQ
        BX1(ID)=PHI
        BX3(ID)=PHI
      ENDIF
 
      RETURN
 
 900  PT2GG(IRP)=0.0
      RETURN
 
C**** END OF ARGING ****************************************************
      END
