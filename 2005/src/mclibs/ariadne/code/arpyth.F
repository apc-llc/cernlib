C***********************************************************************
C $Id: arpyth.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARPYTH

C...ARiadne subroutine perform cascade on PYTHia event

C...Performs a cascade starting on a zero'th order event from PYTHIA


#include "arimpl.f"
#include "arpart.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"
#include "ludat2.f"
#include "pypars.f"
#include "pyint1.f"
#include "leptou.f"

      DIMENSION IR(4)

      ICC(KF)=KCHG(LUCOMP(IABS(KF)),2)*ISIGN(1,KF)
      QDIFF(I,J)=((ABS(K(K(I,3),2)).EQ.210.OR.
     $     ABS(K(K(I,3),2)).EQ.2110.OR.ABS(K(K(I,3),2)).EQ.2210).AND.
     $     K(K(I,3),3).EQ.J)


      IF (LUCOMP(IABS(MSTI(13))).EQ.0) MSTI(13)=K(1,2)
      IF (LUCOMP(IABS(MSTI(14))).EQ.0) MSTI(14)=K(2,2)

C...Check that Ariadne was properly initialized
      IF (MSTA(2).EQ.0.OR.MSTA(1).NE.2) CALL ARERRM('ARPYTH',12,0)

C...Boost to total cms with particl 1 along z-axis
      CALL ARBOPY(THEPY,PHIPY,DBXPY,DBYPY,DBZPY,PHI2PY)

C...Save some parameters that may be changed locally
      ISUB=MSTI(1)
      IFIRST=MSTI(4)+1

C...If we have no colour in the initial state Life is easy
      QH1=(KLU(1,13).NE.0)
      QH2=(KLU(2,13).NE.0)
      IF (ICC(MSTI(13)).EQ.0.AND.ICC(MSTI(14)).EQ.0.AND.
     $     (.NOT.QH1).AND.(.NOT.QH2)) THEN
        IF (ISUB.EQ.25.OR.ISUB.EQ.22) THEN
          CALL ARPYWW
        ELSE
          CALL ARPARS(IFIRST,N)
        ENDIF
        GOTO 900
      ENDIF

C...Check For Drell-Yan type event and make preparations
      CALL ARPRDY

C...Mark up all coloured particles not coming from the hard
C...interaction and save positions of true remnants
      IRQ1=0
      IRD1=0
      IRP1=0
      IRQ2=0
      IRD2=0
      IRP2=0
      PRX1=0.0
      PRY1=0.0
      PRX2=0.0
      PRY2=0.0
      QD1=.FALSE.
      QD2=.FALSE.
      DO 100 I=IFIRST,N
        IF (LUCOMP(IABS(K(I,2))).EQ.0) GOTO 100
        IC=ICC(K(I,2))
        IF (K(I,3).EQ.1.OR.QDIFF(I,1)) THEN
          IF (QDIFF(I,1)) QD1=.TRUE.
          IF (IC.EQ.0) THEN
            IRP1=I
          ELSE
            PRX1=PRX1+P(I,1)
            PRY1=PRY1+P(I,2)
            IF (IC*K(1,2).GT.0) THEN
              IRQ1=I
            ELSE
              IRD1=I
            ENDIF
          ENDIF
        ELSEIF (K(I,3).EQ.2.OR.QDIFF(I,2)) THEN
          IF (QDIFF(I,2)) QD2=.TRUE.
          IF (IC.EQ.0) THEN
            IRP2=I
          ELSE
            PRX2=PRX2+P(I,1)
            PRY2=PRY2+P(I,2)
            IF (IC*K(2,2).GT.0) THEN
              IRQ2=I
            ELSE
              IRD2=I
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE

C...Transfer all dipoles to be cascaded to the Ariadne event record
      IR(1)=IRQ1
      IR(2)=IRD1
      IR(3)=IRQ2
      IR(4)=IRD2
      NSAVE=N
      CALL ARSCAN(IFIRST,NSAVE,4,IR)
      QDUMP=.FALSE.
      
C...Set extendedness of remnants and redistribute momentum if hadron
C...in initial state otherwise special treatment for resolved photon
      IF (QH1) THEN
        IF (.NOT.QD1) CALL ARREMN(1,IR(1),IR(2),IRP1,1)
      ELSE
        JR=MAX(IR(1),IR(2))
        IF (JR.EQ.0.OR.(MHAR(126).EQ.1.AND.
     $       (MINT(107).EQ.3.OR.MINT(107).EQ.0))) THEN
          IF (JR.GT.0) QEX(JR)=.FALSE.
        ELSE
          QEX(JR)=.TRUE.
          XPMU(JR)=PARA(14)*SQRT(PRX1**2+PRY1**2)
          IF (MHAR(128).GT.0)
     $         XPMU(JR)=MAX(XPMU(JR),PARA(14)*ABS(P(1,5)))
          XPA(JR)=PARA(15)
        ENDIF
      ENDIF

      IF (QH2) THEN
        IF (.NOT.QD2) CALL ARREMN(2,IR(3),IR(4),IRP2,-1)
      ELSE
        JR=MAX(IR(3),IR(4))
        IF (JR.EQ.0.OR.(MHAR(126).EQ.1.AND.
     $       (MINT(108).EQ.3.OR.MINT(108).EQ.0))) THEN
          IF (JR.GT.0) QEX(JR)=.FALSE.
        ELSE
          QEX(JR)=.TRUE.
          XPMU(JR)=PARA(14)*SQRT(PRX2**2+PRY2**2)
          IF (MHAR(128).GT.0)
     $         XPMU(JR)=MAX(XPMU(JR),PARA(14)*ABS(P(2,5)))
          XPA(JR)=PARA(15)
        ENDIF
      ENDIF

C...Do special things when DIS lepto-production
      XQ2=-1.0
      XMUST=-1.0
      IF ((MINT(43).EQ.2.OR.MINT(43).EQ.3).AND.
     $     (ISUB.EQ.10.OR.ISUB.EQ.83)) THEN
        X=PARI(34)
        XQ2=-PARI(15)
        XMUST=SQRT(XQ2)*PARA(14)
      ENDIF

C...Perform cascade
      IF (ISUB.EQ.95) THEN
        PT2LST=PHAR(103)*PARP(81)**2
      ELSEIF(ISUB/10.EQ.9.AND.MSTA(34).NE.0) THEN
        PT2LST=MAX(PARP(81)**2,PARI(18))*PHAR(103)
      ELSEIF (MSTA(14).EQ.1) THEN
        PT2LST=PARA(40)
        IF ((ISUB.GE.11.AND.ISUB.LE.17).OR.
     $       (ISUB.GE.28.AND.ISUB.LE.32).OR.
     $       ISUB.EQ.53.OR.ISUB.EQ.68.OR.
     $       (ISUB.GE.80.AND.ISUB.LE.84).OR.
     $       (ISUB.GE.86.AND.ISUB.LE.89).OR.
     $       (ISUB.GE.111.AND.ISUB.LE.113).OR.
     $       ISUB.EQ.115) PT2LST=PARI(18)*PHAR(103)
        IF ((ISUB.EQ.33.OR.ISUB.EQ.34.OR.ISUB.EQ.54).AND.
     $       MHAR(130).EQ.1) PT2LST=PARI(18)*PHAR(103)
      ELSEIF (XMUST.LT.0) THEN
        IF ((ISUB.GE.11.AND.ISUB.LE.17).OR.
     $       (ISUB.GE.28.AND.ISUB.LE.32).OR.
     $       ISUB.EQ.53.OR.ISUB.EQ.68.OR.
     $       (ISUB.GE.80.AND.ISUB.LE.84).OR.
     $       (ISUB.GE.86.AND.ISUB.LE.89).OR.
     $       (ISUB.GE.111.AND.ISUB.LE.113).OR.
     $       ISUB.EQ.115) XMUST=SQRT(PARI(18))*PARA(14)
      ENDIF

C...Set struck quark extended
      IF (MSTA(30).GT.1.AND.XMUST.GT.0) THEN
        DO 110 I=1,IPART
          IF (.NOT.QEX(I)) THEN
            QEX(I)=.TRUE.
            XPMU(I)=XMUST
            XPA(I)=PARA(15)
          ENDIF
 110    CONTINUE
      ENDIF

      CALL ARCASC

C...If multiple interactions, cascade these seperately
      IF (MHAR(133).GT.1.AND.MHAR(133).LT.8) THEN
        IF (MHAR(133).GE.6) NSV=N
        MHAR(133)=-MHAR(133)
        CALL ARPARS(IFIRST,NSAVE)
        MHAR(133)=-MHAR(133)
        IF (MHAR(133).EQ.6) N=NSV
        IF (MHAR(133).EQ.7) THEN
          DO 800 I=1,NSV
            IF (K(I,1).LT.10) K(I,1)=K(I,1)+10
 800      CONTINUE
        ENDIF
      ENDIF

C...If Drell-Yan event fix cascade on decay products
      CALL ARFIDY(NSAVE)

 900  CALL LUDBRB(1,N,0.0,PHI2PY,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(1,N,THEPY,PHIPY,DBXPY,DBYPY,DBZPY)

      RETURN

C**** END OF ARPYTH ****************************************************
      END
C***********************************************************************
C $Id: arpyth.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARPRDY

C...ARiadne subroutine PRepare for Drell-Yan event

C...Check output from PYTHIA for Drell-Yan event and make preparations.


#include "arimpl.f"
#include "arpart.f"
#include "lujets.f"
#include "pypars.f"


      QQ(MAXPAR-2)=.FALSE.
      ISUB=MSTI(1)
      IFIRST=MSTI(4)+1

C...Check which subprocess is active
      ISDY=0
      IF ((ISUB.GT.0.AND.ISUB.LE.8).OR.
     $     (ISUB.GE.14.AND.ISUB.LE.27).OR.
     $     (ISUB.GE.29.AND.ISUB.LE.32).OR.
     $     (ISUB.GE.58.AND.ISUB.LE.67).OR.
     $     (ISUB.GE.69.AND.ISUB.LE.79).OR.
     $     (ISUB.GE.85.AND.ISUB.LE.89).OR.
     $     (ISUB.GE.101.AND.ISUB.LE.144).OR.
     $     (ISUB.GE.151.AND.ISUB.LE.161).OR.
     $     (ISUB.GE.165.AND.ISUB.LE.187)) ISDY=1
      QFISH=.TRUE.
      IF (QFISH.AND.(ISUB.GE.34.AND.ISUB.LE.37).OR.
     $     (ISUB.GE.39.AND.ISUB.LE.42).OR.
     $     (ISUB.GE.44.AND.ISUB.LE.47).OR.
     $     (ISUB.GE.49.AND.ISUB.LE.52).OR.ISUB.EQ.80) ISDY=-1
      IF ((ISUB.EQ.11.AND.MSTI(15)*MSTI(16).LT.0.AND.QFISH).OR.
     $     (ISUB.GE.18.AND.ISUB.LE.27).OR.
     $     (ISUB.GE.58.AND.ISUB.LE.67).OR.
     $     (ISUB.GE.69.AND.ISUB.LE.79).OR.ISUB.EQ.85.OR.ISUB.EQ.110.OR.
     $     ISUB.EQ.114.OR.(ISUB.GE.116.AND.ISUB.LE.119).OR.
     $     (ISUB.GE.165.AND.ISUB.LE.172).OR.
     $     ISUB.EQ.176.OR.ISUB.EQ.177) ISDY=2
      IF ((ISUB.GE.71.AND.ISUB.LE.73).OR.ISUB.EQ.76.OR.ISUB.EQ.77)
     $     ISDY=3

C...In som cases we know which particle is D-Y boson
      ITDY=0
      IF (ISUB.EQ.14.OR.ISUB.EQ.29.OR.ISUB.EQ.34.OR.ISUB.EQ.39.OR.
     $     ISUB.EQ.44.OR.ISUB.EQ.49.OR.ISUB.EQ.115) ITDY=22
      IF (ISUB.EQ.1.OR.ISUB.EQ.7.OR.ISUB.EQ.15.OR.ISUB.EQ.30.OR.
     $     ISUB.EQ.33.OR.ISUB.EQ.40.OR.ISUB.EQ.45.OR.ISUB.EQ.50.OR.
     $     ISUB.EQ.101) ITDY=23
      IF (ISUB.EQ.2.OR.ISUB.EQ.4.OR.ISUB.EQ.6.OR.ISUB.EQ.16.OR.
     $     ISUB.EQ.31.OR.ISUB.EQ.36.OR.ISUB.EQ.41.OR.ISUB.EQ.46.OR.
     $     ISUB.EQ.51) ITDY=24
      IF (ISUB.EQ.3.OR.ISUB.EQ.5.OR.ISUB.EQ.8.OR.ISUB.EQ.17.OR.
     $     ISUB.EQ.32.OR.ISUB.EQ.37.OR.ISUB.EQ.42.OR.ISUB.EQ.47.OR.
     $     ISUB.EQ.52.OR.ISUB.EQ.102.OR.ISUB.EQ.103.OR.ISUB.EQ.111.OR.
     $     ISUB.EQ.112.OR.ISUB.EQ.113.OR.ISUB.EQ.121.OR.ISUB.EQ.122.OR.
     $     ISUB.EQ.123.OR.ISUB.EQ.124) ITDY=25
      IF (ISUB.EQ.141) ITDY=32
      IF (ISUB.EQ.142) ITDY=34
      IF (ISUB.EQ.151.OR.ISUB.EQ.152.OR.ISUB.EQ.153.OR.ISUB.EQ.173.OR.
     $     ISUB.EQ.174.OR.ISUB.EQ.181.OR.ISUB.EQ.182) ITDY=35
      IF (ISUB.EQ.156.OR.ISUB.EQ.157.OR.ISUB.EQ.158.OR.ISUB.EQ.178.OR.
     $     ISUB.EQ.179.OR.ISUB.EQ.186.OR.ISUB.EQ.187) ITDY=36
      IF (ISUB.EQ.143.OR.ISUB.EQ.161) ITDY=37
      IF (ISUB.EQ.144) ITDY=40
      IF (ISUB.EQ.80) ITDY=211
      IF (ISUB.EQ.86) ITDY=443
      IF (ISUB.EQ.87) ITDY=10441
      IF (ISUB.EQ.88) ITDY=20443
      IF (ISUB.EQ.89) ITDY=445

      IF (ISDY.EQ.0) RETURN

      IF (ISDY.EQ.2.OR.ISDY.EQ.3) THEN
C...This is not quite Drell-Yan but the outgoing particles from the
C...hard sub-process constitutes a colour singlet, so combined they
C...should get recoils from initial state and we treat the combined
C...system like a Drell-Yan produced particle
        IF (ISDY.EQ.3) THEN
          I1=9
          I2=10
        ELSE
          I1=7
          I2=8
        ENDIF
        N=N+1
        K(N,1)=11
        K(N,2)=80
        K(N,3)=I1
        K(N,4)=0
        K(N,5)=0
        P(N,1)=P(I1,1)+P(I2,1)
        P(N,2)=P(I1,2)+P(I2,2)
        P(N,3)=P(I1,3)+P(I2,3)
        P(N,4)=P(I1,4)+P(I2,4)
        P(N,5)=SQRT(MAX(P(N,4)**2-P(N,3)**2-P(N,2)**2-P(N,1)**2,0.0))
        K(I1,1)=K(I1,1)+100
        K(I2,1)=K(I2,1)+100
        IDY=N
      ELSE
C...This is Drell-Yan, so find boson
        IF (ITDY.GT.0) THEN
          IDY=IFIRST-1
 900      IDY=IDY+1
          IF (IDY.LE.N.AND.ABS(K(IDY,2)).NE.ITDY) GOTO 900
          IF (IDY.GT.N) CALL ARERRM('ARPYTH',27,0)
        ELSE
          IDY=IFIRST
        ENDIF
      ENDIF

      IF (K(IDY,1).LT.10) THEN
        K(IDY,1)=K(IDY,1)+100
      ELSE
        K(IDY,1)=K(IDY,1)+40
      ENDIF
      DYE=P(IDY,4)
      DYBZ=SQRT(P(IDY,1)**2+P(IDY,2)**2+P(IDY,3)**2)/DYE
      PHIDY=ULANGL(P(IDY,1),P(IDY,2))
      THEDY=ULANGL(P(IDY,3),SQRT(P(IDY,1)**2+P(IDY,2)**2))

C...Find all D-Y Boson decay products, deactivate them and boost to c.m.s
      DO 200 I=K(IDY,3)+1,N
        IF (I.EQ.IDY) GOTO 200
        KPAR=0
        IF (K(I,3).GT.0) KPAR=K(K(I,3),1)
        IF (K(I,3).EQ.IDY.OR.K(I,3).EQ.K(IDY,3).OR.
     $       KPAR.GE.100) THEN
          CALL LUDBRB(I,I,0.0,-PHIDY,0.0D0,0.0D0,0.0D0)
          CALL LUDBRB(I,I,-THEDY,0.0,0.0D0,0.0D0,0.0D0)
          CALL LUDBRB(I,I,0.0,0.0,0.0D0,0.0D0,-DYBZ)
          K(I,1)=K(I,1)+100
        ENDIF
 200  CONTINUE

      CALL ARCOPA(IDY,MAXPAR-2,2)
C...Perform cascade on remnant system (and transfer recoils to D-Y Boson
      QQ(MAXPAR-2)=.TRUE.
      IDI(MAXPAR-2)=IDY

      RETURN

C**** END OF ARPRDY ****************************************************
      END
C***********************************************************************
C $Id: arpyth.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARFIDY(NSAVE)

C...ARiadne subroutine FInish up Drell-Yan event

C...Finish Drell-Yan event performing cascade on D-Y decay products


#include "arimpl.f"
#include "arpart.f"
#include "lujets.f"
#include "pypars.f"


      IF (.NOT.QQ(MAXPAR-2)) RETURN
      QQ(MAXPAR-2)=.FALSE.
      IFIRST=MSTI(4)+1

C...Activate D-Y Boson decay products, boost to new Boson momenta and
C...Perform possible cascade.
      IDY=IDI(MAXPAR-2)
      DYE=P(IDY,4)
      DYBZ=SQRT(P(IDY,1)**2+P(IDY,2)**2+P(IDY,3)**2)/DYE
      PHIDY=ULANGL(P(IDY,1),P(IDY,2))
      THEDY=ULANGL(P(IDY,3),SQRT(P(IDY,1)**2+P(IDY,2)**2))
      DO 210 I=K(IDY,3),NSAVE
        IF (I.EQ.IDY) THEN
          IF (K(I,1).GE.100) K(I,1)=K(I,1)-100
        ELSEIF (K(I,1).GE.100) THEN
          K(I,1)=K(I,1)-100
          CALL LUDBRB(I,I,0.0,0.0,0.0D0,0.0D0,DYBZ)
          CALL LUDBRB(I,I,THEDY,PHIDY,0.0D0,0.0D0,0.0D0)
        ENDIF
 210  CONTINUE
      CALL ARPARS(IFIRST,NSAVE)

      RETURN

C**** END OF ARFIDY ****************************************************
      END
C***********************************************************************
C $Id: arpyth.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARPYWW

C...ARiadne subroutine PYthia WW event

C...Handle a e+e- -> W+W- or e+e- -> Z0Z0 event from PYTHIA

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"
#include "pypars.f"

      DIMENSION IR(1)


      IR(1)=0
      CALL ARSCAN(MSTI(4)+1,N,1,IR)
      MHAR(108)=1
      CALL ARCASC
      IF (PARA(28).GT.0.0.AND.MHAR(101).EQ.2.AND.MSTA(35).EQ.2) THEN
        DO 110 ID=1,IDIPS
          IF (QEM(ID)) GOTO 110
          QDONE(ID)=.FALSE.
          ICOLI(ID)=MOD(ICOLI(ID),1000)
 110    CONTINUE
        IF (MHAR(111).GT.0) THEN
          PARA(28)=-PARA(28)
          PT2LST=PARA(40)
          CALL ARCONT
          PARA(28)=-PARA(28)
        ENDIF
      ENDIF
      MHAR(108)=0

      RETURN

C**** END OF ARPYWW ****************************************************
      END
