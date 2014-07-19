C***********************************************************************
C $Id: arremn.F,v 1.2 1996/04/10 12:33:35 mclareni Exp $

      SUBROUTINE ARREMN(IT,IQR,IDR,IRP,IDIR)

C...ARiadne subroutine FIx REmnants

C...Redistribute remnants and prepare for BGF-like emission


#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "arstrf.f"
#include "arlist.f"
#include "arhide.f"
#include "lujets.f"
#include "ludat1.f"
#include "pypars.f"
#include "pyint3.f"
#include "leptou.f"

      DIMENSION XPQ(-6:6),XPYST(-25:25)


      IF (MHAR(133).GE.5.AND.MHAR(133).LT.9)
     $     CALL ARINCR(IDIR,IQR,IDR,IRP)

      QQ(MAXPAR-5+IT)=.FALSE.
      KFT=K(IT,2)
      FACMU=1.0

C...Get some information from PYTHIA/LEPTO
      IF (MSTA(1).EQ.2) THEN
        KFSTR=K(2+IT,2)
        PARL(4)=0.75
        IF (XQ2.LT.0) THEN
          XSAVE(IT)=PARI(30+IT)
          IF (XSAVE(IT).LE.0.0) THEN
            BRZ=0.0
            BRE=0.0
            IF (IQR.GT.0) THEN
              BRZ=BRZ+BP(IQR,3)
              BRE=BRE+BP(IQR,4)
            ENDIF
            IF (IDR.GT.0) THEN
              BRZ=BRZ+BP(IDR,3)
              BRE=BRE+BP(IDR,4)
            ENDIF
            IF (IRP.GT.0) THEN
              BRZ=BRZ+P(IRP,3)
              BRE=BRE+P(IRP,4)
            ENDIF
            XSAVE(IT)=1.0D0-(BRE+IDIR*BRZ)/(P(IT,4)+IDIR*P(IT,3))
          ENDIF
          XQ2SAV(IT)=PARI(24)
        ELSE
          XSAVE(IT)=X
          XQ2SAV(IT)=XQ2
        ENDIF
        CALL PYSTFU(KFT,XSAVE(IT),XQ2SAV(IT),XPYST)
        DO 10 KF=-6,6
          XPQSAV(IT,KF)=XPYST(KF)
 10     CONTINUE
        XCMU=PARA(10+IT)
      ELSE
        XSAVE(IT)=X
        XQ2SAV(IT)=XQ2
        CALL LNSTRF(X,XQ2,XPQ)
        DO 20 KF=-6,6
          XPQSAV(IT,KF)=XPQ(KF)
 20     CONTINUE
        KFSTR=LST(25)
        XCMU=PARA(11)
      ENDIF
      IF (MSTA(30).NE.0) XCMU=XCMU/(1.0-XSAVE(IT))

C...Check if we hit a Pomeron or something like that
      MHAR(129)=0
      NTRY=0
 200  NTRY=NTRY+1
      IF (MSTA(34).NE.0.AND.XSAVE(IT).GT.0.0) THEN
        KFL=KFSTR
        IF (KFSTR.EQ.21) KFL=0
        IF (MSTA(34).GT.0) THEN
          CALL ARPOSF(KFT,KFL,XSAVE(IT),XQ2SAV(IT),
     $         XPOM,TPOM,KFTF,KFPR,XPQSAV(IT,KFL),XFPOM)
        ELSE
          CALL ARUPOM(KFT,KFL,XSAVE(IT),XQ2SAV(IT),
     $         XPOM,TPOM,KFTF,KFPR,XPQSAV(IT,KFL),XFPOM)
        ENDIF
        IF (XFPOM.GT.XPQSAV(IT,KFL)*PARA(18)) THEN
          IF (NTRY.LE.1) CALL ARERRM('ARREMN',29,0)
          XFPOM=XPQSAV(IT,KFL)*PARA(18)
        ENDIF

        IF (XFPOM.GT.RLU(IDUM)*XPQSAV(IT,KFL).AND.NTRY.LT.10000) THEN
          CALL ARPOKI(IT,IQR,IDR,IRP,IDIR,KFTF,KFPR,XPOM,TPOM,QFAIL)
          IF (QFAIL) GOTO 200
          IF (MSTA(1).EQ.3) MSTA(33)=0
          MHAR(129)=1
          RETURN
        ENDIF
      ENDIF

C...If this quark was in fact a gluon or if non-baryonic target -
C...just fix extendedness
      IF (MOD(KFT/1000,10).EQ.0.OR.KFSTR.EQ.21.OR.
     $     XSAVE(IT).LE.0.0) THEN
        IF (IDR.GT.0) THEN
          QEX(IDR)=.TRUE.
          XPA(IDR)=PARA(10)
          XPMU(IDR)=XCMU
          Z=1.0
          PTI=SQRT(BP(IDR,1)**2+BP(IDR,2)**2)
          IF (IQR.GT.0) Z=(BP(IDR,4)+IDIR*BP(IDR,3))/
     $         (BP(IDR,4)+IDIR*BP(IDR,3)+BP(IQR,4)+IDIR*BP(IQR,3))
          IF (IRP.GT.0) Z=(BP(IDR,4)+IDIR*BP(IDR,3))/
     $         (BP(IDR,4)+IDIR*BP(IDR,3)+P(IRP,4)+IDIR*P(IRP,3))
          IF (MHAR(115).NE.0.OR.ABS(MSTA(36)).EQ.1) THEN
            XPMU(IDR)=XCMU/Z
          ELSE
            XPA(IDR)=PARA(15)
            IF (MSTA(36).EQ.2) XPMU(IDR)=PARA(14)*PTI
            IF (MSTA(36).EQ.3) XPMU(IDR)=PARA(14)*PTI/(1.0-XSAVE(IT))
            IF (MSTA(36).EQ.4)
     $           XPMU(IDR)=PARA(14)*PTI/(Z*(1.0-XSAVE(IT)))
          ENDIF
        ENDIF
        IF (IQR.GT.0) THEN
          QEX(IQR)=.TRUE.
          XPA(IQR)=PARA(10)
          XPMU(IQR)=XCMU
          Z=1.0
          PTI=SQRT(BP(IQR,1)**2+BP(IQR,2)**2)
          IF (IDR.GT.0) Z=(BP(IQR,4)+IDIR*BP(IQR,3))/
     $         (BP(IDR,4)+IDIR*BP(IDR,3)+BP(IQR,4)+IDIR*BP(IQR,3))
          IF (IRP.GT.0) Z=(BP(IQR,4)+IDIR*BP(IQR,3))/
     $         (BP(IQR,4)+IDIR*BP(IQR,3)+P(IRP,4)+IDIR*P(IRP,3))
          IF (MHAR(115).NE.0.OR.ABS(MSTA(36)).EQ.1) THEN
            XPMU(IQR)=XCMU/Z
          ELSE
            XPA(IQR)=PARA(15)
            IF (MSTA(36).EQ.2) XPMU(IQR)=PARA(14)*PTI
            IF (MSTA(36).EQ.3) XPMU(IQR)=PARA(14)*PTI/(1.0-XSAVE(IT))
            IF (MSTA(36).EQ.4)
     $           XPMU(IQR)=PARA(14)*PTI/(Z*(1.0-XSAVE(IT)))
          ENDIF
        ENDIF

        RETURN
      ENDIF

C...Get valens quark flavours from baryon
      KFTA=ABS(KFT)
      KFQ1=SIGN(KFTA/1000,KFT)
      KFTA=MOD(KFTA,1000)
      KFQ2=SIGN(KFTA/100,KFT)
      KFTA=MOD(KFTA,100)
      KFQ3=SIGN(KFTA/10,KFT)
      QSEA=(KFSTR.NE.KFQ1.AND.KFSTR.NE.KFQ2.AND.KFSTR.NE.KFQ3)
      QSEA=(QSEA.OR.(XPQSAV(IT,KFSTR)-XPQSAV(IT,-KFSTR))/
     $     (XPQSAV(IT,KFSTR)+XPQSAV(IT,-KFSTR)).LT.RLU(0))

C...Sum up remnant momentum
      IR=0
      IF (IQR.GT.0) THEN
        IF (IDR.GT.0) CALL ARERRM('ARREMN',30,0)
        IR=IQR
      ELSE
        IF (IDR.LE.0) CALL ARERRM('ARREMN',30,0)
        IR=IDR
      ENDIF
      BRX=BP(IR,1)
      BRY=BP(IR,2)
      BRZ=BP(IR,3)
      BRE=BP(IR,4)
      IF (IRP.GT.0) THEN
        BRX=BRX+P(IRP,1)
        BRY=BRY+P(IRP,2)
        BRZ=BRZ+P(IRP,3)
        BRE=BRE+P(IRP,4)
      ENDIF
      IRPJ=IRP
      IF (IRPJ.GT.0) K(IRPJ,4)=0
      IRP=-IRP

C...Scale mu for remnant if multiple interactions
      IF (MHAR(133).EQ.4.AND.PHAR(131).GT.0.AND.IDIR.GT.0) THEN
        FACMU=(PHAR(131)+BRZ+BRE)/(BRZ+BRE)
      ELSEIF (MHAR(133).EQ.4.AND.PHAR(132).GT.0.AND.IDIR.LT.0) THEN
        FACMU=(PHAR(132)-BRZ+BRE)/(BRE-BRZ)
      ENDIF

C...Select particle to fix momentum transfer with
      IF (QQ(MAXPAR-2)) THEN
        ISQ=MAXPAR-2
      ELSEIF(IDI(IR).GT.0) THEN
        ISQ=IP1(IDI(IR))
      ELSE
        ISQ=IP3(IDO(IR))
      ENDIF

C...Rotate to Z-axis and calculate boost
      DBZ=(BRZ+BP(ISQ,3))/(BRE+BP(ISQ,4))
      S=(BRE+BP(ISQ,4))**2-(BRZ+BP(ISQ,3))**2-
     $     (BRY+BP(ISQ,2))**2-(BRX+BP(ISQ,1))**2
      
      IF (S.LT.0) CALL ARERRM('ARREMN',25,0)

      IF (QSEA) THEN
        IRP=MAXPAR-5+IT
        IF (IRPJ.GT.0) K(IRPJ,4)=-IRP
        QQ(IRP)=QSEA
C...Select diquark
        NTRY=0
 100    NTRY=NTRY+1
        IF (NTRY.GT.10000) THEN
          CALL ARERRM('ARREMN',22,0)
          RETURN
        ENDIF
        QIDENT=.FALSE.
        RND=RLU(0)
        IF (RND.LT.1.0/3.0) THEN
          KFQ=KFQ1
          KFR=MAX(ABS(KFQ2),ABS(KFQ3))*1000+
     $         MIN(ABS(KFQ2),ABS(KFQ3))*100+3
          QIDENT=(KFQ2.EQ.KFQ3)
        ELSEIF (RND.LT.2.0/3.0) THEN
          KFQ=KFQ2
          KFR=MAX(ABS(KFQ1),ABS(KFQ3))*1000+
     $         MIN(ABS(KFQ1),ABS(KFQ3))*100+3
          QIDENT=(KFQ1.EQ.KFQ3)
        ELSE
          KFQ=KFQ3
          KFR=MAX(ABS(KFQ1),ABS(KFQ2))*1000+
     $         MIN(ABS(KFQ1),ABS(KFQ2))*100+3
          QIDENT=(KFQ1.EQ.KFQ2)
        ENDIF

C...If non-identical  quarks, make diquark spinless with probability
C...PARL(4)
        IF ((.NOT.QIDENT).AND.RLU(IDUM).LT.PARL(4)) KFR=KFR-2
        IF (KFQ.LT.0) KFR=-KFR

C...Get flavour of particle
        IF (KFQ*KFSTR.LT.0) THEN
          CALL LUKFDI(KFR,-KFSTR,IDUM,KFP)
          KFREM1=KFQ
          KFREM2=KFR
        ELSE
          CALL LUKFDI(KFQ,-KFSTR,IDUM,KFP)
          KFREM1=KFR
          KFREM2=KFQ
        ENDIF

        IF (KFP.EQ.0) GOTO 100

C...  Assign z of hadron
        CALL LUPTDI(KFREM2,PTX,PTY)

        IF (MSTA(37).EQ.0) THEN
          IF (MSTA(1).EQ.2) THEN
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
        ELSEIF (MSTA(37).EQ.1) THEN
          PTI=PARA(27)*SQRT(-LOG(RLU(0)))
        ELSEIF (MSTA(37).EQ.2) THEN
          RPT1=RLU(0)
          RPT2=RLU(0)
          PTI=-PARA(27)*LOG(RPT1*RPT2)/SQRT(6.0)          
        ENDIF
        PHII=PARU(2)*RLU(0)
        PTIX=PTI*COS(PHII)
        PTIY=PTI*SIN(PHII)

        RMR1=ULMASS(KFREM1)
        RMP=ULMASS(KFP)
        RMTSQ=SQRT(BP(ISQ,5)**2+(BP(ISQ,1)+BRX-PTIX-PTX)**2+
     $       (BP(ISQ,2)+BRY-PTIY-PTY)**2)
        RMT2R1=RMR1**2+PTIX**2+PTIY**2
        RMT2P=RMP**2+PTX**2+PTY**2
        CALL LUZDIS(KFREM2,0,RMT2P,Z)
        RMR=SQRT(RMT2P/Z+RMT2R1/(1.0-Z))
        PT2SQ=(BRX+BP(ISQ,1))**2+(BRY+BP(ISQ,2))**2
        PZTOT=ARZCMS(S+PT2SQ,RMR,RMTSQ)
        IF (PZTOT.LE.0.0) GOTO 100
        PP=SQRT(PZTOT**2+RMR**2)+PZTOT

C...Set info for chopped off hadron
        IFL(IRP)=KFP
        INO(IRP)=KFREM2
        INQ(IRP)=IR
        IDI(IRP)=ISQ
        IDO(IRP)=KFSTR
        BP(IRP,1)=PTX
        BP(IRP,2)=PTY
        BP(IRP,3)=IDIR*0.5*(Z*PP-RMT2P/(Z*PP))
        BP(IRP,4)=0.5*(Z*PP+RMT2P/(Z*PP))
        BP(IRP,5)=RMP
        QEX(IRP)=.TRUE.
        XPA(IRP)=PARA(10)
        XPMU(IRP)=XCMU*FACMU
        PT2GG(IRP)=-1.0
        IF (ABS(MSTA(36)).EQ.1) XPMU(IRP)=FACMU*XCMU/Z
        IF (MSTA(36).EQ.2) XPMU(IRP)=FACMU*PARA(14)*PTI
        IF (MSTA(36).EQ.3) XPMU(IRP)=FACMU*PARA(14)*PTI/(1.0-XSAVE(IT))
        IF (MSTA(36).EQ.4) XPMU(IRP)=FACMU*PARA(14)*PTI/
     $       (Z*(1.0-XSAVE(IT)))
        IRDIR(IT)=IDIR

C...Set info for remnant
        IFL(IR)=KFREM1
        BP(IR,1)=PTIX
        BP(IR,2)=PTIY
        BP(IR,3)=IDIR*0.5*((1.0-Z)*PP-RMT2R1/((1.0-Z)*PP))
        BP(IR,4)=0.5*((1.0-Z)*PP+RMT2R1/((1.0-Z)*PP))
        BP(IR,5)=RMR1
        QEX(IR)=.TRUE.
        XPA(IR)=PARA(10)
        XPMU(IR)=FACMU*XCMU
        IF (ABS(MSTA(36)).EQ.1) XPMU(IR)=FACMU*XCMU/(1.0-Z)
        IF (MSTA(36).EQ.2) XPMU(IR)=FACMU*PARA(14)*PTI
        IF (MSTA(36).EQ.3) XPMU(IR)=FACMU*PARA(14)*PTI/(1.0-XSAVE(IT))
        IF (MSTA(36).EQ.4) XPMU(IR)=FACMU*PARA(14)*PTI/
     $       ((1.0-XSAVE(IT))*(1.0-Z))

C...Fix momentum for struck particle
        BP(ISQ,1)=BP(ISQ,1)+BRX-PTX-PTIX
        BP(ISQ,2)=BP(ISQ,2)+BRY-PTY-PTIY
        BP(ISQ,3)=-BP(IR,3)-BP(IRP,3)
        BP(ISQ,4)=SQRT(BP(ISQ,1)**2+BP(ISQ,2)**2+
     $       BP(ISQ,3)**2+BP(ISQ,5)**2)

C...boost back
        CALL AROBO3(0.0,0.0,0.0D0,0.0D0,DBZ,ISQ,IR,IRP)

      ELSE

C...We have a valens-quark interaction
C...Get flavor of diquark
 110    IF (KFQ2.EQ.KFSTR) THEN
          KFQ=KFQ2
          KFQ2=KFQ1
          KFQ1=KFQ
        ELSEIF (KFQ3.EQ.KFSTR) THEN
          KFQ=KFQ3
          KFQ3=KFQ1
          KFQ1=KFQ
        ENDIF
        KFR=SIGN(MAX(ABS(KFQ2),ABS(KFQ3))*1000+
     $       MIN(ABS(KFQ2),ABS(KFQ3))*100+3,KFQ1)
        IF (KFQ2.NE.KFQ3.AND.RLU(0).LT.PARL(4))
     $       KFR=SIGN(ABS(KFR)-2,KFR)

        RMR=ULMASS(KFR)
        
        IF (MSTA(37).EQ.0) THEN
          IF (MSTA(1).EQ.2) THEN
            IF(MSTP(91).LE.0) THEN
              PTI=0.
            ELSEIF(MSTP(91).EQ.1) THEN
              PTI=PARP(91)*SQRT(-LOG(RLU(0)))
            ELSE
              RPT1=RLU(0)
              RPT2=RLU(0)
              PTI=-PARP(92)*LOG(RPT1*RPT2)
            ENDIF
            IF(PTI.GT.PARP(93)) GOTO 110
          ELSE
            PTI=PARL(3)*SQRT(-LOG(RLU(0)))
          ENDIF
        ELSEIF (MSTA(37).EQ.1) THEN
          PTI=PARA(27)*SQRT(-LOG(RLU(0)))
        ELSEIF (MSTA(37).EQ.2) THEN
          RPT1=RLU(0)
          RPT2=RLU(0)
          PTI=-PARA(27)*LOG(RPT1*RPT2)/SQRT(6.0)          
        ENDIF
        PHII=PARU(2)*RLU(0)
        PTIX=PTI*COS(PHII)
        PTIY=PTI*SIN(PHII)

        RMTSQ=SQRT(BP(ISQ,5)**2+(BP(ISQ,1)+BRX-PTIX)**2+
     $       (BP(ISQ,2)+BRY-PTIY)**2)
        RMT2R=RMR**2+PTI**2
        RMTR=SQRT(RMT2R)
        PT2SQ=(BP(ISQ,1)+BRX)**2+(BP(ISQ,2)+BRY)**2
        PZTOT=ARZCMS(S+PT2SQ,RMTR,RMTSQ)
        IF (PZTOT.LT.0.0) GOTO 110

C...Set info for remnant
        IFL(IR)=KFR
        BP(IR,1)=PTIX
        BP(IR,2)=PTIY
        BP(IR,3)=IDIR*PZTOT
        BP(IR,4)=SQRT(PZTOT**2+RMT2R)
        BP(IR,5)=RMR
        QEX(IR)=.TRUE.
        XPMU(IR)=FACMU*XCMU
        IF (MSTA(36).EQ.2) XPMU(IR)=FACMU*PARA(14)*PTI
        IF (MSTA(36).GE.3) XPMU(IR)=FACMU*PARA(14)*PTI/(1.0-XSAVE(IT))
        XPA(IR)=PARA(10)

C...Fix momentum for struck particle
        BP(ISQ,1)=BP(ISQ,1)+BRX-PTIX
        BP(ISQ,2)=BP(ISQ,2)+BRY-PTIY
        BP(ISQ,3)=-BP(IR,3)
        BP(ISQ,4)=SQRT(BP(ISQ,1)**2+BP(ISQ,2)**2+
     $       BP(ISQ,3)**2+BP(ISQ,5)**2)

C...Boost back
        CALL AROBO2(0.0,0.0,0.0D0,0.0D0,DBZ,ISQ,IR)

      ENDIF
        
      RETURN

C**** END OF ARREMN ****************************************************
      END
C***********************************************************************
C $Id: arremn.F,v 1.2 1996/04/10 12:33:35 mclareni Exp $

      SUBROUTINE ARINCR(IDIR,IQR,IDR,IRP)

C...ARiadne subroutine FIx REmnants

C...Redistribute remnants and prepare for BGF-like emission


#include "arimpl.f"
#include "arpart.f"
#include "arhide.f"
#include "lujets.f"


      DIMENSION IPRT(4)

      IF (IDIR.GT.0) THEN
        IF(PHAR(131).LE.0) RETURN
        BPX=PHAR(131)
      ELSEIF (IDIR.LT.0) THEN
        IF(PHAR(132).LE.0) RETURN
        BPX=PHAR(132)
      ENDIF

      NPRT=0
      BPR=0.0
      IF (IQR.GT.0) THEN
        BPR=BPR+BP(IQR,4)+IDIR*BP(IQR,3)
        NPRT=NPRT+1
        IPRT(NPRT)=IQR
      ENDIF
      IF (IDR.GT.0) THEN
        BPR=BPR+BP(IDR,4)+IDIR*BP(IDR,3)
        NPRT=NPRT+1
        IPRT(NPRT)=IDR
      ENDIF
      IF (IRP.GT.0) THEN
        BPR=BPR+P(IRP,4)+IDIR*P(IRP,3)
      ENDIF

      DPN2=(BPR+BPX)**2
      DPO2=BPR**2
      DBZ=IDIR*(DPN2-DPO2)/(DPN2+DPO2)

      CALL ARROBO(0.0,0.0,0.0D0,0.0D0,DBZ,NPRT,IPRT)
      IF (IRP.GT.0)
     $     CALL LUDBRB(IRP,IRP,0.0,0.0,0.0D0,0.0D0,DBZ)

      RETURN
      END

