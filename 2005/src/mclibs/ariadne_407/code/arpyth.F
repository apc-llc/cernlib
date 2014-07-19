*
* $Id: arpyth.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arpyth.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpyth.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARPYTH
 
C...ARiadne subroutine perform cascade on PYTHia event
 
C...Performs a cascade starting on a zero'th order event from PYTHIA
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
      COMMON /PYINT1/ MINT(400),VINT(400)
      SAVE /PYINT1/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
 
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
