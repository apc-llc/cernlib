*
* $Id: arprdy.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arprdy.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arprdy.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARPRDY
 
C...ARiadne subroutine PRepare for Drell-Yan event
 
C...Check output from PYTHIA for Drell-Yan event and make preparations.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
 
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
