*
* $Id: arorie.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arorie.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arorie.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
 
      SUBROUTINE ARORIE(I1,I2,I3,BS,B1,B3,QR1,QR3,PT21,PT23)
 
C...ARiadne subroutine ORIEnt
 
C...Orients three partons according to recoil strategy determined
C...by QR1 and QR3
 
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
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      INXT(I)=IP3(IDO(I))
      IPRV(I)=IP1(IDI(I))
 
 
C...Set parton energies and momentum and total energy
      BW=SQRT(BS)
      IF (B1.LE.0.0) CALL ARERRM('ARORIE',9,0)
      DE1=0.5*B1*BW
      IF (B3.LE.0.0) CALL ARERRM('ARORIE',9,0)
      DE3=0.5*B3*BW
      DE2=BW-DE1-DE3
      IF (DE2.LT.BP(I2,5)) CALL ARERRM('ARORIE',9,0)
      DP1=SQRT(DE1**2-BP(I1,5)**2)
      DP2=SQRT(DE2**2-BP(I2,5)**2)
      DP3=SQRT(DE3**2-BP(I3,5)**2)
 
C...If both partons 1 and 3 can take full recoil choose one according to
C...Kleiss
      IF (QR1.AND.QR3) THEN
        IF (B1**2.LT.(B1**2+B3**2)*RLU(IDUM)) THEN
          QR1=.FALSE.
        ELSE
          QR3=.FALSE.
        ENDIF
      ENDIF
 
C...Calculate angle between partons 1 and 3
      BCALP=1.0
      IF (DP1.GT.0.0.AND.DP3.GT.0.0) THEN
        BCALP=(DP2**2-DP1**2-DP3**2)/(2.0*DP1*DP3)
      ELSE
        CALL ARERRM('ARORIE',9,0)
      ENDIF
      IF (ABS(BCALP).GT.1.0) CALL ARERRM('ARORIE',9,0)
      BCALP=MAX(-1.0D0,MIN(1.0D0,DBLE(BCALP)))
      BALP=ACOS(BCALP)
 
C...Determine angle between parton 1 and z-axis
      IF (QR1.AND.PT21.LE.0.0.AND.PT23.LE.0.0) THEN
        BPSI=PARU(1)-BALP
      ELSEIF (QR3.AND.PT21.LE.0.0.AND.PT23.LE.0.0) THEN
        BPSI=0.0
      ELSE
        BPSI=(PARU(1)-BALP)*(B3**2)/(B1**2+B3**2)
 
C...New recoil strategy
        IF (PT21.GT.0.0.AND.PT21.GE.PT23) THEN
          I0=IPRV(I1)
          BPSI=ARECOI(BP(I0,4),DE1,DE2,DE3,ABS(BP(I0,3)),DP1,DP2,DP3,
     $         BALP,PT21)
        ELSEIF (PT23.GT.0.0.AND.PT23.GT.PT21) THEN
          I4=INXT(I3)
          BPSI=PARU(1)-BALP-
     $         ARECOI(BP(I4,4),DE3,DE2,DE1,ABS(BP(I4,3)),
     $         DP3,DP2,DP1,BALP,PT23)
        ENDIF
      ENDIF
 
C...Set random azimuth angle
      BGAM=PARU(2)*RLU(IDUM)
      BSGAM=SIN(BGAM)
      BCGAM=COS(BGAM)
      BSPSI=SIN(BPSI)
      BCPSI=COS(BPSI)
      BSPSA=SIN(BPSI+BALP)
      BCPSA=COS(BPSI+BALP)
 
C...Set fourmomentum of partons
      BP(I1,1)=DP1*BSPSI*BSGAM
      BP(I1,2)=-DP1*BSPSI*BCGAM
      BP(I1,3)=DP1*BCPSI
      BP(I1,4)=DE1
 
      BP(I3,1)=DP3*BSPSA*BSGAM
      BP(I3,2)=-DP3*BSPSA*BCGAM
      BP(I3,3)=DP3*BCPSA
      BP(I3,4)=DE3
 
      DZ2=-DP1*BCPSI-DP3*BCPSA
      DT2=DSQRT(MAX(DP2**2-DZ2**2,0.0D0))
      BP(I2,1)=-DT2*BSGAM
      BP(I2,2)=DT2*BCGAM
      BP(I2,3)=DZ2
      BP(I2,4)=DE2
 
      RETURN
 
C**** END OF ARORIE ****************************************************
      END
