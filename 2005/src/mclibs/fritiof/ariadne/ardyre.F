*
* $Id: ardyre.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: ardyre.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: ardyre.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARDYRE(ID,*)
 
C...ARiadne subroutine Drell-Yan REcoil treatment
 
C...Transfers the recoil of an emission to a Drell-Yan produced
C...particle if the emission and the particle are in the same
C...phase space region.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Locate Drell-Yan produced particle (IDY) and boost it to CMS
C...of dipole
      IDY=MSTA(23)
      CALL LUDBRB(IDY,IDY,0.0,0.0,-DBEX,-DBEY,-DBEZ)
      CALL LUDBRB(IDY,IDY,0.0,-PHI,0.0D0,0.0D0,0.0D0)
      CALL LUDBRB(IDY,IDY,-THE,0.0,0.0D0,0.0D0,0.0D0)
 
C...Calculate p_t and y for emitted gluon and light cone momenta for
C...IDY
      PTG=SQRT(PT2IN(ID))
      ZG=SQRT((1.0-BX1(ID))/(1.0-BX3(ID)))
      BPDY=P(IDY,4)+P(IDY,3)
      BMDY=P(IDY,4)-P(IDY,3)
 
C...If gluon is 'outside' IDYs phase-space, exit and perform normal
C...emission
      IF(PTG.GT.BPDY*BMDY/(BMDY*ZG+BPDY/ZG)) THEN
        CALL LUDBRB(IDY,IDY,THE,PHI,DBEX,DBEY,DBEZ)
        RETURN
      ENDIF
 
C...Calculate positions of particles in imitting dipole and emitted
C...gluon
      I1=IP1(ID)
      I3=IP3(ID)
      CALL ARADDG(ID)
      IG=IP3(ID)
 
C...Set momenta for gluon
      BPG=PTG*ZG
      BMG=PTG/ZG
 
      W=SQRT(SDIP(ID))
      BPTOT=W+BPDY
      BMTOT=W+BMDY
 
      BET=PARU(2)*RLU(IDUM)
 
      BP(IG,1)=PTG*SIN(BET)
      BP(IG,2)=PTG*COS(BET)
      BP(IG,3)=0.5*(BPG-BMG)
      BP(IG,4)=0.5*(BPG+BMG)
      BP(IG,5)=0.0
 
C...Transfer transverse recoil to IDY and set new momenta for IDY
      P(IDY,1)=P(IDY,1)-BP(IG,1)
      P(IDY,2)=P(IDY,2)-BP(IG,2)
      XMT2=(P(IDY,1)**2+P(IDY,2)**2+P(IDY,5)**2)/(BPDY*BMDY)
      BPDY=BPDY*SQRT(XMT2)
      BMDY=BMDY*SQRT(XMT2)
      P(IDY,3)=0.5*(BPDY-BMDY)
      P(IDY,4)=0.5*(BPDY+BMDY)
 
      BPTOT=BPTOT-BPDY-BPG
      BMTOT=BMTOT-BMDY-BMG
 
C...Set new momenta for particles in emitting dipole and exit if
C...the recoil transfer is not kinematically allowed
      Y1=BP(I1,5)**2
      Y3=BP(I3,5)**2
 
      IF(BMTOT.LT.1.0E-20) CALL ARERRM('ARDYRE',11,0)
 
      BB=0.5*(BPTOT*BMTOT+Y1-Y3)/BMTOT
      BA=Y1*BPTOT/BMTOT
 
      IF(BB**2-BA.LT.0.0) CALL ARERRM('ARDYRE',11,0)
 
      BP1=BB+SQRT(BB**2-BA)
 
      IF(BP1.LE.SQRT(Y1)) CALL ARERRM('ARDYRE',11,0)
      BM1=Y1/BP1
 
      BM3=BMTOT-BM1
 
      IF(BM3.LE.SQRT(Y3)) CALL ARERRM('ARDYRE',11,0)
      BP3=Y3/BM3
 
      BP(I1,1)=0.0
      BP(I1,2)=0.0
      BP(I1,3)=0.5*(BP1-BM1)
      BP(I1,4)=0.5*(BP1+BM1)
 
      BP(I3,1)=0.0
      BP(I3,2)=0.0
      BP(I3,3)=0.5*(BP3-BM3)
      BP(I3,4)=0.5*(BP3+BM3)
 
C...Boost back all particles to original system
      CALL LUDBRB(IDY,IDY,THE,PHI,DBEX,DBEY,DBEZ)
      CALL AROBO3(THE,PHI,DBEX,DBEY,DBEZ,I1,IG,I3)
 
      RETURN 1
 
C**** END OF ARDYRE ****************************************************
      END
