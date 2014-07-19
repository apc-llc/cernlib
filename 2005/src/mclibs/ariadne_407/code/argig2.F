*
* $Id: argig2.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: argig2.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argig2.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARGIG2(ID,IRP)
 
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
 
 
      IF (MHAR(102).LT.0) RETURN
 
      QEXDIS=((MSTA(1).EQ.3.AND.IO.EQ.0.AND.MHAR(120).GT.0).OR.
     $     (MSTA(1).EQ.2.AND.IO.EQ.0.AND.
     $     XQ2.GT.0.0.AND.MHAR(120).GT.0))
 
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
      KQ=IDO(IRP)
      KF=K(IT,2)
      RMQ=ULMASS(KQ)
      DMQ2=RMQ**2
      IT=IRP+5-MAXPAR
      IDIR=IRDIR(IT)
      PM=P(IT,4)+IDIR*P(IT,3)
      PT2CUT=MAX(PARA(3)**2,PT2IN(ID))/PHAR(103)
      IF (QEXDIS) PT2CUT=MAX(PARA(3)**2+SNGL(DMQ2),PT2IN(ID))/PHAR(103)
 
      IF (NPTOT.EQ.0) THEN
        DO 10 I=1,IPART
          IPTOT(I)=I
 10     CONTINUE
        NPTOT=IPART
      ENDIF
 
      NPSTQ=0
      DO 20 I=1,NPTOT
        IF (INO(IPTOT(I)).EQ.0) THEN
          IF (INQ(IPTOT(I)).GE.0) GOTO 20
          IF (K(-INQ(IPTOT(I)),3).LE.2) GOTO 20
        ENDIF
        NPSTQ=NPSTQ+1
        IPSTQ(NPSTQ)=IPTOT(I)
 20   CONTINUE
      IF (MSTA(1).NE.2.OR.IDI(IRP).GT.IPART)
     $     CALL ARPADD(-IDI(IRP),NPSTQ,IPSTQ)
 
      QEXDY=((MHAR(120).GT.0.AND.NPSTQ.EQ.1.AND.IPSTQ(1).EQ.MAXPAR-2)
     $     .OR.(MHAR(124).EQ.2.AND.
     $     ((NPSTQ.EQ.1.AND.IPSTQ(1).EQ.MAXPAR-2).OR.NPSTQ.GT.1)))
 
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
 
      DSTOT=(DER+DEQ)**2-(DZR+DZQ)**2-(DYR+DYQ)**2-(DXR+DXQ)**2
      DMS2=DMQ**2
 
      XX=1.0-(DER+IDIR*DZR)/PM
      IF (QEXDY) XX=DMS2/DSTOT
      IF (QEXDIS) XX=X
 
      PT2MX=MIN(REAL(((DSTOT+DMQ2-DMS2)**2)/(4.0*DSTOT)-DMQ2),PT2LST)
      SMT2MX=PT2MX+DMQ2
      SMT2CT=PT2CUT+DMQ2
 
      IF (QEXDIS) PT2MX=PT2MX+DMQ2
 
      IF (PT2MX.LE.PT2CUT) GOTO 900
 
      IF (QEXDIS) THEN
        SQ2MAX=0.5*(XQ2+PT2MX)
        SQ2MIN=PT2CUT/
     $       ((0.5+SQRT(MAX(0.25-PT2CUT*X/(XQ2*(1.0-X)),0.0)))*(1.0-X))
      ELSE
        SQ2MAX=DSTOT-DMQ2-DMS2
        SQ2MIN=0.5*(DSTOT-DMQ2-DMS2-
     $       SQRT((DSTOT-DMQ2-DMS2)**2-4.0*(DMQ2*DMS2+PT2CUT*DSTOT)))
      ENDIF
      SQ2MIN=MAX(SQ2MIN,REAL(DMQ2)+PHAR(109))
      IF (ABS(KQ).EQ.4) SQ2MIN=MAX(SQ2MIN,2.56+PHAR(109))
 
      XNUMFL=MAX(ARNOFL(SQRT(SQ2MAX),MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)
 
      IF (MHAR(127).EQ.0) THEN
        STRA0=ARSTRA(KF,KQ,XX,1.0,SQ2MAX)
        IF (STRA0.LE.0.0) THEN
          GOTO 900
        ENDIF
        IF (MHAR(118).GT.0) THEN
          STRA0=STRA0*REAL(MHAR(118))
        ELSEIF (MHAR(118).LT.0) THEN
          STRA0=2.0*STRA0/(1.0-XX)
          IF (ABS(KQ).GE.4) STRA0=STRA0*ABS(REAL(MHAR(118)))
        ELSE
          STRA0=MAX(STRA0,ARSTRA(KF,KQ,XX,1.0,SQ2MIN))
        ENDIF
      ELSE
        STRA0=ARSTRA(KF,KQ,XX,1.0,REAL(MHAR(127))*SMT2MX)
        IF (STRA0.LE.0.0) THEN
          GOTO 900
        ENDIF
        STRA0=MAX(STRA0,ARSTRA(KF,KQ,XX,1.0,REAL(MHAR(127))*SMT2CT))
      ENDIF
 
      C=PHAR(104)*ALPHA0*STRA0/PARU(1)
      ZINT=1.0-XX
      CN=1.0/(C*ZINT)
      XLAM2=(PARA(1)**2)/PHAR(103)
      IF (QEXDY) THEN
        SQARG=1.0-4.0*(PT2CUT+DMQ2)*DSTOT/((DSTOT+DMQ2-DMS2)**2)
        XIINT=LOG((1.0+SQRT(SQARG))/(1.0-SQRT(SQARG)))
        CN=1.0/(C*XIINT)
      ELSEIF (QEXDIS) THEN
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
        XI=ZQ
        IF (XI.GE.1.0) GOTO 100
        IF (SQRT((PT2MX+DMQ2*(1.0-XI)+DMS2*XI)/(XI*(1.0-XI))).GE.
     $       SQRT(DSTOT)-DMR) GOTO 100
      ELSEIF (QEXDY) THEN
        XIMAX=0.5*(DSTOT+DMQ2-DMS2+
     $       SQRT((DSTOT+DMQ2-DMS2)**2-4.0*(PT2MX+DMQ2)*DSTOT))/DSTOT
        XIMIN=0.5*(DSTOT+DMQ2-DMS2-
     $       SQRT((DSTOT+DMQ2-DMS2)**2-4.0*(PT2MX+DMQ2)*DSTOT))/DSTOT
        XI=XIMIN*((XIMAX/XIMIN)**RLU(0))
        SH=(PT2MX+DMQ2*(1.0-XI)+DMS2*XI)/(XI*(1.0-XI))
        TH=-(PT2MX+DMS2*XI)/(1.0-XI)
        UH=DMS2+DMQ2-SH-TH
        SQ2=-TH
        IF (SQRT(DSTOT).LE.SQRT(SH)+DMR) GOTO 100
        Z=DMS2/SH
        IF (MHAR(124).GT.0) THEN
          PMR=ARPCMS(REAL(DSTOT),REAL(DMR),SQRT(SH))
          PMR0=ARPCMS(REAL(DSTOT),REAL(DMR),REAL(DMQ))
          Z=XX/(1.0-(1.0-XX)*PMR/PMR0)
        ENDIF
        W=0.25*PT2MX/(PT2MX+XI*DMS2)
        IF (MHAR(120).EQ.1) W=W*Z
        IF (MHAR(125).EQ.940801) W=W*2.0
        W=W*LOG(XIMAX/XIMIN)/XIINT
        W=W*(SH**2+TH**2+2.0*DMS2*UH)/(SH**2)
      ELSE
        Z=XX+RLU(0)*(1.0-XX)
        W=(Z**2+(1.0-Z)**2)*0.25
        SQ2=PT2MX/(1.0-Z)
        XI=(DMQ2+PT2MX)/(XX*(1.0/Z-1.0)*DSTOT)
        IF (MHAR(124).EQ.1) THEN
          AARG=DSTOT*XX*(1.0-Z)+DMS2*(Z-XX)
          BARG=(DMS2-DMQ2)*Z*(1.0-XX)-AARG
          CARG=(DMQ2+PT2MX)*(1.0-XX)*Z
          SQARG=BARG**2-4.0*AARG*CARG
          IF (SQARG.LT.0.0) GOTO 100
          XI=0.5*(-BARG-SQRT(SQARG))/AARG
          IF (XI.LE.0.0) GOTO 100
        ELSEIF (MHAR(124).EQ.3) THEN
          XI=(SQ2+DMQ2)/((1.0-(1.0-XX/Z)/(1.0-XX))*(DSTOT-DMS2))
        ENDIF
        IF (XI.GE.1.0) GOTO 100
        IF (SQRT((PT2MX+DMQ2*(1.0-XI)+DMS2*XI)/(XI*(1.0-XI))).GE.
     $       SQRT(DSTOT)-DMR) GOTO 100
      ENDIF
 
      SQ2=MAX(SQ2,SQ2MIN)
      IF (MHAR(127).EQ.0) THEN
        STRA=ARSTRA(KF,KQ,XX,Z,SQ2)
      ELSE
        SMT2MX=PT2MX+DMQ2
        IF (QEXDIS) SMT2MX=PT2MX
        STRA=ARSTRA(KF,KQ,XX,Z,REAL(MHAR(127))*SMT2MX)
      ENDIF
      IF (STRA.LT.0.0) GOTO 100
      W=W*STRA/STRA0
 
      IF (W.LT.RLU(IDUM)) GOTO 100
 
      IF (QEXDIS) THEN
        YQ=ZQ
        XA=0.125*(1.0+(1.0-XY)**2)*(Z**2+(1.0-Z)**2)*
     $       (ZQ**2+(1.0-ZQ)**2)/(ZQ*(1.0-ZQ))+2.0*(1.0-XY)*Z*(1.0-Z)
        XB=0.5*XY*SQRT((1.0-XY)*Z*(1.0-Z)/(ZQ*(1.0-ZQ)))*
     $       (1.0-2.0/XY)*(1.0-2.0*ZQ)*(1.0-2.0*Z)
        XC=(1.0-XY)*Z*(1.0-Z)
        ABC=ABS(XA)+ABS(XB)+ABS(XC)
 200    PHI=PARU(2)*RLU(IDUM)
        IF (XA+XB*COS(PHI)+XC*COS(2.0*PHI).LT.RLU(IDUM)*ABC) GOTO 200
      ELSE
        YQ=XI
        PHI=PARU(2)*RLU(IDUM)
      ENDIF
 
      NTT=NTT+1
      IF (W.GT.1.0) THEN
        NTE1=NTE1+1
        WRITE(0,*) REAL(NTE1)/REAL(NTT),REAL(NTE2)/REAL(NTT)
      ENDIF
      IF (STRA.GT.STRA0) THEN
        NTE2=NTE2+1
      ENDIF
 
      IF (QEXDIS) PT2MX=PT2MX-DMQ2
 
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
 
C**** END OF ARGIG2 ****************************************************
      END
