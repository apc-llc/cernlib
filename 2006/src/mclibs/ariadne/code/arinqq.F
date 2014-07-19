C***********************************************************************
C $Id: arinqq.F,v 1.2 1996/04/10 12:33:19 mclareni Exp $

      SUBROUTINE ARINQQ(IT,KQ,IRP,PT2,YQ,ZQ,PHI,QFAIL)

C...ARiadne perform INitial state g->QQ

C...Try to perform an initial-state g->qqbar splitting.


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstrf.f"
#include "ardat1.f"
#include "arhide.f"

      DIMENSION ISTQ(MAXPAR),IREM(MAXPAR)


      IF (MHAR(104).GT.0) THEN
        CALL ARNIQQ(IT,KQ,IRP,PT2,YQ,PHI,QFAIL)
        RETURN
      ENDIF

      QFAIL=.TRUE.
      RMQ2=ULMASS(KQ)
      IF (MHAR(102).GE.1) THEN
        DMT2Q=PT2
        DPT2Q=DMT2Q-RMQ2**2
      ELSE
        DPT2Q=PT2
        DMT2Q=DPT2Q+RMQ2**2
      ENDIF
      
C...First divide up all partons into remnant and struck system
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
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NREM,IREM)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)

C...Check that emission is possible
      SR=DMR**2
      S=MAX(0.0D0,(DER+DEQ)**2-(DZR+DZQ)**2-
     $     (DYR+DYQ)**2-(DXR+DXQ)**2)
      SQ1=DMQ**2
      SQ=(RMQ2**2)/ZQ+SQ1/(1.0-ZQ)+DPT2Q/(ZQ*(1.0-ZQ))
      IF (SQRT(S).LT.SQRT(SQ)+SQRT(SR)) RETURN

C...Boost to CMS with struck system on Z-axis
      CALL ARSUME(0,DBTX,DBTY,DBTZ,DBTE,DMTM,NREM,IREM)
      CALL ARSUME(1,DBTX,DBTY,DBTZ,DBTE,DMTM,NSTQ,ISTQ)
      DBTZ=DBTZ/DBTE
      DBTY=DBTY/DBTE
      DBTX=DBTX/DBTE
      CALL ARROBO(0.0,0.0,-DBTX,-DBTY,-DBTZ,NREM,IREM)
      CALL ARROBO(0.0,0.0,-DBTX,-DBTY,-DBTZ,NSTQ,ISTQ)
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)
      PHIT=ULANGL(SNGL(DXQ),SNGL(DYQ))
      THET=ULANGL(SNGL(DZQ),SNGL(SQRT(DXQ**2+DYQ**2)))
      CALL ARROBO(0.0,-PHIT,0.0D0,0.0D0,0.0D0,NREM,IREM)
      CALL ARROBO(0.0,-PHIT,0.0D0,0.0D0,0.0D0,NSTQ,ISTQ)
      CALL ARROBO(-THET,0.0,0.0D0,0.0D0,0.0D0,NREM,IREM)
      CALL ARROBO(-THET,0.0,0.0D0,0.0D0,0.0D0,NSTQ,ISTQ)

      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)
C...Rotate struck system to correct PT
      DPP0=DZQ+DEQ
      DPT=SQRT(DPT2Q)
      CALL ARDBRB(-ASIN(DPT/DZQ),DBLE(PHI-PHIT),
     $     0.0D0,0.0D0,0.0D0,NSTQ,ISTQ)

C...Boost to correct momentum
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)
      DPP02=(DZQ+DEQ)**2
      DPP2=((1.0D0-DBLE(ZQ))*DPP0)**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DPP2-DPP02)/(DPP2+DPP02),NSTQ,ISTQ)

C...Insert new quark
      IO=IO+1
      IPART=IPART+1
      IFL(IPART)=-KQ
      IF (MSTA(30).LT.2.OR.MSTA(30).EQ.3) THEN
        QEX(IPART)=.FALSE.
        XPMU(IPART)=0.0
        XPA(IPART)=0.0
        QEX(IQ)=.FALSE.
        XPMU(IQ)=0.0
        XPA(IQ)=0.0
      ELSE
        QEX(IPART)=.TRUE.
        IF (PARA(14).GE.0) THEN
          XPMU(IPART)=SQRT(XQ2SAV(IT))*PARA(14)
        ELSE
          XPMU(IPART)=ABS(PARA(14))
        ENDIF
        XPA(IPART)=PARA(15)
      ENDIF
      QEX(IPART)=.FALSE.
      QQ(IPART)=.TRUE.
      INO(IPART)=IO
      INQ(IPART)=0
      DPP=ZQ*DPP0
      BP(IPART,5)=RMQ2
      BP(IPART,4)=0.5*(DPP+DMT2Q/DPP)
      BP(IPART,3)=0.5*(DPP-DMT2Q/DPP)
      BP(IPART,2)=-DYQ
      BP(IPART,1)=-DXQ
      NSTQ=NSTQ+1
      ISTQ(NSTQ)=IPART

C...Insert new remnant
      IPART=IPART+1
      IFL(IPART)=INO(IRP)
      QEX(IPART)=QEX(IRP)
      QQ(IPART)=.TRUE.
      INO(IPART)=0
      INQ(IPART)=0
      XPMU(IPART)=XPMU(IRP)
      XPA(IPART)=XPA(IRP)
      BP(IPART,1)=BP(IRP,1)
      BP(IPART,2)=BP(IRP,2)
      BP(IPART,3)=BP(IRP,3)
      BP(IPART,4)=BP(IRP,4)
      BP(IPART,5)=BP(IRP,5)
      IREM(1)=IPART
      QQ(IRP)=.FALSE.

C...Sum up energy again
      CALL ARSUME(0,DXQ,DYQ,DZQ,DEQ,DMQ,NSTQ,ISTQ)
      CALL ARSUME(0,DXR,DYR,DZR,DER,DMR,NREM,IREM)

C...Boost around to fix up energy
      IF (SQRT(S).LT.DMQ+DMR) THEN
        CALL ARERRM('ARINQQ',25,0)
        GOTO 900
      ENDIF
      DPQ2=ARPCMS(S,SNGL(DMQ),SNGL(DMR))**2
      DPQ02=(DEQ+DZQ)**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DPQ2-DPQ02)/(DPQ2+DPQ02),NSTQ,ISTQ)
      DMR2=ARPCMS(S,SNGL(DMR),SNGL(DMQ))**2
      DMR02=(DER-DZR)**2
      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,
     $     (DMR02-DMR2)/(DMR02+DMR2),NREM,IREM)

C...Boost and rotate back to original system
      CALL ARROBO(THET,PHIT,DBTX,DBTY,DBTZ,NREM,IREM)
      CALL ARROBO(THET,PHIT,DBTX,DBTY,DBTZ,NSTQ,ISTQ)

C...Finally fix mass of second remnant and connect new dipole
      IQ2=IPART-1
      IR=IPART
      DE=BP(IQ2,4)+BP(IR,4)
      DBEX=(BP(IQ2,1)+BP(IR,1))/DE
      DBEY=(BP(IQ2,2)+BP(IR,2))/DE
      DBEZ=(BP(IQ2,3)+BP(IR,3))/DE
      CALL AROBO2(0.0,0.0,-DBEX,-DBEY,-DBEZ,IQ2,IR)
      PX=BP(IQ2,1)
      PY=BP(IQ2,2)
      PZ=BP(IQ2,3)
      PHIT=ULANGL(PX,PY)
      THET=ULANGL(PZ,SQRT(PX**2+PY**2))
      CALL AROBO2(0.0,-PHIT,0.0D0,0.0D0,0.0D0,IQ2,IR)
      CALL AROBO2(-THET,0.0,0.0D0,0.0D0,0.0D0,IQ2,IR)
      RMR=ULMASS(IFL(IR))
      RMQ=BP(IQ2,5)
      BZ=ARZCMS(ARMAS2(IQ2,IR),RMQ,RMR)
      IF (BZ.LT.0.0) THEN
        CALL ARERRM('ARINQQ',10,0)
      ELSE
        BP(IQ2,3)=BZ
        BP(IQ2,4)=SQRT(BZ**2+BP(IQ2,5)**2)
        BP(IR,5)=RMR
        BP(IR,4)=SQRT(BZ**2+RMR**2)
        BP(IR,3)=-BZ
      ENDIF

      CALL AROBO2(THET,PHIT,DBEX,DBEY,DBEZ,IQ2,IR)

      IDIPS=IDIPS+1
      ISTRS=ISTRS+1
      CALL ARCRDI(IDIPS,IQ2,IR,ISTRS,.FALSE.)
      IDI(IQ2)=0
      IDO(IR)=0
      IPF(ISTRS)=IQ2
      IPL(ISTRS)=IR
      IFLOW(ISTRS)=SIGN(1,-KQ)

      QFAIL=.FALSE.

 900  DO 300 ID=1,IDIPS
        QDONE(ID)=.FALSE.
 300  CONTINUE

      RETURN

C**** END OF ARINQQ ****************************************************
      END
