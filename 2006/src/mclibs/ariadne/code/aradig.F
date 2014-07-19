C***********************************************************************
C $Id: aradig.F,v 1.2 1996/04/10 12:33:01 mclareni Exp $

      SUBROUTINE ARADIG(ID)

C...Ariadne RADiater Initial state G->qq

C...Perform an initial-state g->qqbar splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstrf.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"
#include "leptou.f"
#include "lujets.f"


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
C***********************************************************************
C $Id: aradig.F,v 1.2 1996/04/10 12:33:01 mclareni Exp $

      SUBROUTINE ARADG2(ID)

C...Ariadne RADiater initial state G->qq

C...Perform an initial-state g->qqbar splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstrf.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"
#include "leptou.f"
#include "lujets.f"


      IF (ABS(MSTA(33)).EQ.1.AND.MSTA(1).EQ.3.AND.IO.EQ.1) THEN
        LST(24)=3
      ENDIF
      IRP=IRAD(ID)-10000
      IT=IRP+5-MAXPAR
      IR=INQ(IRP)
      NPREM=2
      IPREM(1)=IRP
      IPREM(2)=IR
      KQ=IDO(IRP)
      RMQ=ULMASS(KQ)
      DMQ2=RMQ**2
      DPT2Q=PT2IQQ(IT)
      XI=AEX1(ID)
      PHIQ=BX1(ID)

      NPSTQ=0
      DO 110 I=1,IPART
        IF (INO(I).EQ.0) THEN
          IF (INQ(I).GE.0) GOTO 110
          IF (K(-INQ(I),3).LE.2) GOTO 110
        ENDIF
        NPSTQ=NPSTQ+1
        IPSTQ(NPSTQ)=I
 110  CONTINUE
      IF (MSTA(1).NE.2.OR.IDI(IRP).GT.IPART)
     $     CALL ARPADD(-IDI(IRP),NPSTQ,IPSTQ)

      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)

      DBEX=(DXR+DXQ)/(DER+DEQ)
      DBEY=(DYR+DYQ)/(DER+DEQ)
      DBEZ=(DZR+DZQ)/(DER+DEQ)

      CALL ARROBO(0.0,0.0,-DBEX,-DBEY,-DBEZ,NPSTQ,IPSTQ)
      CALL ARROBO(0.0,0.0,-DBEX,-DBEY,-DBEZ,NPREM,IPREM)
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)
      DSTOT=(DER+DEQ)**2-(DZR+DZQ)**2-(DYR+DYQ)**2-(DXR+DXQ)**2
      PHI=ULANGL(REAL(DXQ),REAL(DYQ))
      THE=ULANGL(REAL(DZQ),REAL(SQRT(DXQ**2+DYQ**2)))
      CALL ARROBO(0.0,-PHI,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
      CALL ARROBO(0.0,-PHI,0.0D0,0.0D0,0.0D0,NPREM,IPREM)
      CALL ARROBO(-THE,0.0,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
      CALL ARROBO(-THE,0.0,0.0D0,0.0D0,0.0D0,NPREM,IPREM)
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NPREM,IPREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NPSTQ,IPSTQ)

      DMS2=DMQ**2

      DSH=(DPT2Q+DMQ2*(1.0-XI)+DMS2*XI)/(XI*(1.0-XI))

      DPMR=ARPCMS(REAL(DSTOT),REAL(DMR),REAL(SQRT(DSH)))
      DPMR0=DER-DZR
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DPMR0**2-DPMR**2)/(DPMR0**2+DPMR**2),NPREM,IPREM)

      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,-DZQ/DEQ,NPSTQ,IPSTQ)
      CALL ARROBO(0.0,PHI-PHIQ,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)
      CALL ARROBO(0.0,0.0,-DSQRT(DPT2Q)/DSQRT(DPT2Q+DMQ**2),
     $     0.0D0,0.0D0,NPSTQ,IPSTQ)
      DPPTOT=SQRT(DSH)
      DPPQ=(1.0-XI)*DPPTOT
      DPPQ2=XI*DPPTOT
      DPPQ02=DPT2Q+DMQ**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DPPQ**2-DPPQ02)/(DPPQ**2+DPPQ02),NPSTQ,IPSTQ)
      
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
      BP(IQ2,1)=SQRT(DPT2Q)
      BP(IQ2,2)=0.0
      BP(IQ2,3)=0.5*(DPPQ2-(DPT2Q+DMQ2)/DPPQ2)
      BP(IQ2,4)=0.5*(DPPQ2+(DPT2Q+DMQ2)/DPPQ2)
      BP(IQ2,5)=RMQ

      NPSTQ=NPSTQ+1
      IPSTQ(NPSTQ)=IQ2

      CALL ARROBO(0.0,PHIQ-PHI,0.0D0,0.0D0,0.0D0,NPSTQ,IPSTQ)

      DZ=ARZCMS(REAL(DSTOT),REAL(SQRT(DSH)),REAL(DMR))
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,DZ/SQRT((DZ**2+DSH)),NPSTQ,IPSTQ)

C...Insert new remnant
 100  IPART=IPART+1
      IR=IPART
      IPREM(1)=IR
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

      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,NPSTQ,IPSTQ)
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,NPREM,IPREM)

      CALL ARCHEM(0)

      IF (IO.EQ.1) THEN
        PHAR(121)=0.5*LOG(MAX(BP(IQ2,4)+BP(IQ2,3),1.0D-30)/
     $       MAX(BP(IQ2,4)-BP(IQ2,3),1.0D-30))
        PHAR(122)=BP(IQ2,1)**2+BP(IQ2,2)**2
      ENDIF

      RETURN

C**** END OF ARADG2 ****************************************************
      END
