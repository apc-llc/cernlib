C***********************************************************************
C $Id: arging.F,v 1.2 1996/04/10 12:33:15 mclareni Exp $

      SUBROUTINE ARGING(ID,IRP)

C...ARiadne Generate INitial state G->QQ

C...Generate kinematical variables describing an initial-state g->qqbar
C...splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"
#include "ludat1.f"
#include "leptou.f"

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
C***********************************************************************
C $Id: arging.F,v 1.2 1996/04/10 12:33:15 mclareni Exp $

      SUBROUTINE ARGIG2(ID,IRP)

C...ARiadne Generate INitial state G->QQ

C...Generate kinematical variables describing an initial-state g->qqbar
C...splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"
#include "ludat1.f"
#include "leptou.f"

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
