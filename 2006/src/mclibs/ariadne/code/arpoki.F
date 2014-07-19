C***********************************************************************
C $Id: arpoki.F,v 1.2 1996/04/10 12:33:29 mclareni Exp $

      SUBROUTINE ARPOKI(IT,IQR,IDR,IRP,IDIR,KFTF,KFPR,XPOM,TPOM,QFAIL)

C...ARiadne subroutine POmeron KInematics

C...Redistribute remnants assuming an incoming hadron IT, outgoing
C...hadron KFTF with a pomeron (or other colourless  sub object) with
C...momentum fraction XPOM and viruality -TPOM giving a remnant of
C...flavour KFPR. IQR, IDR and IRP as in subroutine ARREMN


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "lujets.f"
#include "ludat1.f"
#include "leptou.f"
#include "pypars.f"


      QFAIL=.TRUE.

C...Sum up remnant momentum
      BRX=0.0
      BRY=0.0
      BRZ=0.0
      BRE=0.0
      IF (IQR.GT.0) THEN
        IF (IDR.GT.0.AND.KFPR.NE.21) CALL ARERRM('ARPOKI',30,0)
        BRX=BRX+BP(IQR,1)
        BRY=BRY+BP(IQR,2)
        BRZ=BRZ+BP(IQR,3)
        BRE=BRE+BP(IQR,4)
      ENDIF
      IF (IDR.GT.0) THEN
        IF (IQR.GT.0.AND.KFPR.NE.21) CALL ARERRM('ARPOKI',30,0)
        BRX=BRX+BP(IDR,1)
        BRY=BRY+BP(IDR,2)
        BRZ=BRZ+BP(IDR,3)
        BRE=BRE+BP(IDR,4)
      ENDIF
      IF (IRP.GT.0) THEN
        IF (IQR.GT.0.AND.IDR.GT.0) CALL ARERRM('ARPOKI',30,0)
        BRX=BRX+P(IRP,1)
        BRY=BRY+P(IRP,2)
        BRZ=BRZ+P(IRP,3)
        BRE=BRE+P(IRP,4)
      ENDIF

      IF (KFPR.EQ.21) THEN
        IF (IQR.LE.0.OR.IDR.LE.0) CALL ARERRM('ARPOKI',30,0)
        IR=MIN(IQR,IDR)
      ELSE
        IR=MAX(IQR,IDR)
      ENDIF

C...Select particle to fix momentum transfer with
      IF (QQ(MAXPAR-2)) THEN
        ISQ=MAXPAR-2
      ELSEIF (IDI(IR).GT.0) THEN
        IF (IDO(IR).GT.0.AND.QEX(IP1(IDI(IR)))) THEN
          ISQ=IP3(IDO(IR))
        ELSE
          ISQ=IP1(IDI(IR))
        ENDIF
      ELSE
        ISQ=IP3(IDO(IR))
      ENDIF

      RM2PI=P(IT,5)**2
      RMPF=ULMASS(KFTF)
      RM2PF=RMPF**2
      PT2PF=TPOM*(1.0-XPOM)+XPOM*(RM2PI*(1.0-XPOM)-RM2PF)
      IF (PT2PF.LT.0.0) RETURN

      PHIPF=PARU(2)*RLU(0)
      PTXPF=SQRT(PT2PF)*COS(PHIPF)
      PTYPF=SQRT(PT2PF)*SIN(PHIPF)
      PPI=P(IT,4)+IDIR*P(IT,3)
      PPF=(1.0-XPOM)*PPI
      RMT2PF=RM2PF+PT2PF
      K(N+1,1)=1
      K(N+1,2)=KFTF
      K(N+1,3)=IT
      K(N+1,4)=0
      K(N+1,5)=0
      P(N+1,1)=PTXPF
      P(N+1,2)=PTYPF
      P(N+1,3)=IDIR*0.5*(PPF-RMT2PF/PPF)
      P(N+1,4)=0.5*(PPF+RMT2PF/PPF)
      P(N+1,5)=RMPF

C...Calculate boost to struck pomeron CMS and check kinematics
      
      RMPR=ULMASS(KFPR)
      RMT2PR=RMPR**2
      RMT2SQ=(BRX+BP(ISQ,1)-PTXPF)**2+
     $     (BRY+BP(ISQ,2)-PTYPF)**2+BP(ISQ,5)**2
      BSE=BRE+BP(ISQ,4)-P(N+1,4)
      BSZ=BRZ+BP(ISQ,3)-P(N+1,3)
      BSY=BRY+BP(ISQ,2)-P(N+1,2)
      BSX=BRX+BP(ISQ,1)-P(N+1,1)
      S=BSE**2-BSZ**2-BSY**2-BSX**2
      DBZ=BSZ/BSE
      
      IF (S.LT.0) RETURN
      BW=SQRT(S)
      IF (SQRT(RMT2PR)+SQRT(RMT2SQ).GE.BW) RETURN

 100  IF (PARA(16).GT.0) THEN
        PTI=PARA(16)*SQRT(-LOG(RLU(0)))
        IF (PTI.GT.PARA(17)) GOTO 100
      ELSEIF (MSTA(1).EQ.2) THEN
        IF(MSTP(91).LE.0) THEN
          PTI=0.
        ELSEIF(MSTP(91).EQ.1) THEN
          PTI=PARP(91)*SQRT(-LOG(RLU(0)))
        ELSE
          RPT1=RLU(0)
          RPT2=RLU(0)
          PTI=-PARP(92)*LOG(RPT1*RPT2)
        ENDIF
        IF(PTI.GT.PARP(93)) GOTO 100
      ELSE
        PTI=PARL(3)*SQRT(-LOG(RLU(0)))
      ENDIF
      PHII=PARU(2)*RLU(0)
      PTIX=PTI*COS(PHII)
      PTIY=PTI*SIN(PHII)

      RMT2PR=PTI**2+RMPR**2
      RMTPR=SQRT(RMT2PR)
      RMT2SQ=(BSX-PTIX)**2+(BSY-PTIY)**2+BP(ISQ,5)**2
      RMTSQ=SQRT(RMT2SQ)
      IF (RMTPR+RMTSQ.GE.BW) GOTO 100
      PT2SQ=BSX**2+BSY**2
      PZTOT=ARZCMS(S+PT2SQ,RMTSQ,RMTPR)
      IF (PZTOT.LT.0.0) GOTO 100

      BP(ISQ,1)=BSX-PTIX
      BP(ISQ,2)=BSY-PTIY
      BP(ISQ,3)=-IDIR*PZTOT
      BP(ISQ,4)=SQRT(PZTOT**2+RMT2SQ)

      IF (KFPR.EQ.21) CALL ARJOQQ(IQR,IDR)

      BP(IR,1)=PTIX
      BP(IR,2)=PTIY
      BP(IR,3)=IDIR*PZTOT
      BP(IR,4)=SQRT(PZTOT**2+RMT2PR)
      BP(IR,5)=RMPR

      IFL(IR)=KFPR
      QEX(IR)=.TRUE.
      XPMU(IR)=PARA(14)*MAX(PTI,SQRT(TPOM))
      XPA(IR)=PARA(15)
      PT2GG(IR)=0.0
      IF (IDI(IR).GT.0) QDONE(IDI(IR))=.FALSE.
      IF (IDO(IR).GT.0) QDONE(IDO(IR))=.FALSE.

      CALL AROBO2(0.0,0.0,0.0D0,0.0D0,DBZ,ISQ,IR)

      N=N+1
      IF (IRP.GT.0) THEN
        IF (K(IRP,1).LT.10) K(IRP,1)=K(IRP,1)+10
        IRP=-IRP
      ENDIF
      QFAIL=.FALSE.

      RETURN

C**** END OF ARPOKI ****************************************************
      END
