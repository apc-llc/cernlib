C***********************************************************************
C $Id: arniqq.F,v 1.2 1996/04/10 12:33:24 mclareni Exp $

      SUBROUTINE ARNIQQ(IT,KQ,IRP,PT2,YQ,PHIQ,QFAIL)

C...ARiadne perform INitial state g->QQ

C...Try to perform an initial-state g->qqbar splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstrf.f"
#include "ardat1.f"
#include "arhide.f"
#include "ludat1.f"

      DIMENSION ISTQ(MAXPAR),IREM(MAXPAR),ITOT(MAXPAR)


      QFAIL=.TRUE.
      RMQ2=ULMASS(KQ)
      IF (MHAR(102).GE.1) THEN
        DMT2Q=PT2
        DPT2Q=DMT2Q-RMQ2**2
      ELSE
        DPT2Q=PT2
        DMT2Q=DPT2Q+RMQ2**2
      ENDIF

C...First boost all particle to total cms
      DO 100 I=1,IPART
        ITOT(I)=I
 100  CONTINUE
      NI=IPART
      DO 110 I=2,4
        IF (QQ(MAXPAR-I)) THEN
          NI=NI+1
          ITOT(NI)=MAXPAR-I
        ENDIF
 110  CONTINUE
      CALL ARSUME(0,DXT,DYT,DZT,DET,DMT,NI,ITOT)
      DBXT=DXT/DET
      DBYT=DYT/DET
      DBZT=DZT/DET
      CALL ARROBO(0.0,0.0,-DBXT,-DBYT,-DBZT,NI,ITOT)
      
C...Then divide up all partons into remnant and struck system
      NREM=2
      IR=INQ(IRP)
      IREM(1)=IRP
      IREM(2)=IR
      NSTQ=1
      IQ=IDI(IRP)
      ISTQ(1)=IQ
C...      DO 100 I=1,IPART
C...        IF (I.EQ.IRP.OR.I.EQ.IR.OR.I.EQ.IQ) GOTO 100
C...        IF (INO(I).LT.0.OR.QEX(I)) THEN
C...          NREM=NREM+1
C...          IREM(NREM)=I
C...        ELSE
C...          NSTQ=NSTQ+1
C...          ISTQ(NSTQ)=I
C...        ENDIF
C... 100  CONTINUE

C...Now rotate remnant system to negative z-axis and quark in x-z plane
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NREM,IREM)
      PHIT=ULANGL(REAL(DXR),REAL(DYR))
      THET=ULANGL(REAL(DZR),REAL(SQRT(DXR**2+DYR**2)))-PARU(1)
      CALL ARROBO(0.0,-PHIT,0.0D0,0.0D0,0.0D0,NI,ITOT)
      CALL ARROBO(-THET,0.0,0.0D0,0.0D0,0.0D0,NI,ITOT)
      CALL ARROBO(0.0,-PHIQ,0.0D0,0.0D0,0.0D0,NI,ITOT)

C...Check that emission is possible
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NREM,IREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)

      BXQ=SQRT(DPT2Q)
      BYQ=0.0
      BZQ=SQRT(DMT2Q)*SINH(YQ)
      BEQ=SQRT(DMT2Q)*COSH(YQ)
      BQP=BEQ+BZQ
      BQM=BEQ-BZQ

      B0P=DEQ+DZQ
      B0M=DEQ-DZQ
      BM0D2=DMQ**2+(DXQ-BXQ)**2+(DYQ-BYQ)**2

      BRP=DER+DZR
      BRM=DER-DZR

      BRQP=B0P+BRP-BQP
      BRQM=B0M+BRM-BQM

      BA=(BRQP*BRQM+BRP*BRM-BM0D2)/(2.0*BRQM*BRP)
      BB=BRM*BRQP/(BRP*BRQM)

      IF (BA**2.LT.BB.OR.BA.LE.0.0.OR.BRQP.LE.0.0.OR.BRQM.LE.0.0)
     $     GOTO 900

      DAR=BA-SQRT(BA**2-BB)

      IF (DAR.LE.1.0) GOTO 900

      DXZQ=SIGN(SQRT(DXQ**2+DZQ**2),DZQ)
      IF (ABS(DXZQ).LE.ABS(DXQ-BXQ)) GOTO 900

C...Boost remnant system to correct rapidity
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DAR**2-1.0D0)/(DAR**2+1.0D0),NREM,IREM)

C...Rotate struck system to right pt
      CALL ARROBO(REAL(ASIN((DXQ-BXQ)/DXZQ)-ASIN(DXQ/DXZQ)),
     $     0.0,0.0D0,0.0D0,0.0D0,NSTQ,ISTQ)

C...Boost struck system to right rapidity
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)
      DPP2=(BRP*(1.0-DAR)+B0P-BQP)**2
      DPP02=(DZQ+DEQ)**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,(DPP2-DPP02)/(DPP2+DPP02),
     $     NSTQ,ISTQ)

C...Insert new quark
      IO=IO+1
      IPART=IPART+1
      IQ2=IPART
      IFL(IQ2)=-KQ
      IF (MSTA(30).LT.2.OR.MSTA(30).EQ.3) THEN
        QEX(IQ2)=.FALSE.
        XPMU(IQ2)=0.0
        XPA(IQ2)=0.0
        QEX(IQ)=.FALSE.
        XPMU(IQ)=0.0
        XPA(IQ)=0.0
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
      BP(IQ2,4)=BEQ
      BP(IQ2,3)=BZQ
      BP(IQ2,2)=BYQ
      BP(IQ2,1)=BXQ
      NI=NI+1
      ITOT(NI)=IQ2

C...Insert new remnant
      IPART=IPART+1
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
      NI=NI+1
      ITOT(NI)=IR

C...Fix new string and dipole
      IDIPS=IDIPS+1
      ISTRS=ISTRS+1
      CALL ARCRDI(IDIPS,IQ2,IR,ISTRS,.FALSE.)
      IDI(IQ2)=0
      IDO(IR)=0
      IPF(ISTRS)=IQ2
      IPL(ISTRS)=IR
      IFLOW(ISTRS)=SIGN(1,-KQ)

      QFAIL=.FALSE.

C...Reset all dipole flags
 900  DO 200 ID=1,IDIPS
        QDONE(ID)=.FALSE.
 200  CONTINUE

C...Boost back
      CALL ARROBO(0.0,PHIQ,0.0D0,0.0D0,0.0D0,NI,ITOT)
      CALL ARROBO(THET,PHIT,DBXT,DBYT,DBZT,NI,ITOT)

      RETURN

C**** END OF ARNIQQ ****************************************************
      END
