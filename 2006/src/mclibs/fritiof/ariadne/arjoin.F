*
* $Id: arjoin.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arjoin.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arjoin.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARJOIN(J1,J2,J3)
 
C...ARiadne subroutine JOIN jets
 
C...Join three jets into two
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Copy jets into ariadne dipole record
      CALL ARCOPA(J1,1,1)
      CALL ARCOPA(J2,2,1)
      CALL ARCOPA(J3,3,1)
 
C...Boost to CMS of jets
      DE=BP(1,4)+BP(2,4)+BP(3,4)
      DBEX=(BP(1,1)+BP(2,1)+BP(3,1))/DE
      DBEY=(BP(1,2)+BP(2,2)+BP(3,2))/DE
      DBEZ=(BP(1,3)+BP(2,3)+BP(3,3))/DE
 
      CALL AROBO3(0.0,0.0,-DBEX,-DBEY,-DBEZ,1,2,3)
 
C...Rotate Jet 1 to z-axis and jet 2 to xz plane
      PX=BP(1,1)
      PY=BP(1,2)
      PZ=BP(1,3)
      PHI=ULANGL(PX,PY)
      CALL AROBO3(0.0,-PHI,0.0D0,0.0D0,0.0D0,1,2,3)
      THE=ULANGL(PZ,SQRT(PX**2+PY**2))
      CALL AROBO3(-THE,0.0,0.0D0,0.0D0,0.0D0,1,2,3)
      PX=BP(2,1)
      PY=BP(2,2)
      PHI2=ULANGL(PX,PY)
      CALL AROBO3(0.0,-PHI2,0.0D0,0.0D0,0.0D0,1,2,3)
 
C...Calculate energy fractions
      BE=BP(1,4)+BP(2,4)+BP(3,4)
      B1=2.0*BP(1,4)/BE
      B3=2.0*BP(3,4)/BE
 
C...Determine recoil angle
      BET=ARANGL(1,3)
      PSI=(PARU(1)-BET)*(B3**2)/(B1**2+B3**2)
      BP(1,1)=0.0
      BP(1,2)=0.0
      BP(1,3)=BE*0.5
      BP(1,4)=BE*0.5
      BP(1,5)=0.0
      BP(2,1)=0.0
      BP(2,2)=0.0
      BP(2,3)=-BE*0.5
      BP(2,4)=BE*0.5
      BP(2,5)=0.0
 
C...Rotate and boost back
      CALL AROBO2(PSI,0.0,0.0D0,0.0D0,0.0D0,1,2)
      CALL AROBO2(0.0,PHI2,0.0D0,0.0D0,0.0D0,1,2)
      CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,1,2)
 
C...Copy jets to /LUJETS/
      DO 100 J=1,5
        P(J1,J)=BP(1,J)
        P(J3,J)=BP(2,J)
 100  CONTINUE
      V(J1,1)=0.0
      V(J3,1)=0.0
 
      RETURN
 
C**** END OF ARJOIN ****************************************************
      END
