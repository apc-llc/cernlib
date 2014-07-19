*
* $Id: arexma.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: arexma.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arexma.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
 
      SUBROUTINE AREXMA(I1,I3)
 
C...ARiadne subroutine make EXtended partons MAssless
 
C...Makes extended partons massless.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
      IF(MSTA(31).GT.0) RETURN
      IF(IEX(I1).EQ.0.AND.IEX(I3).EQ.0) RETURN
      DPE1=BP(I1,4)
      DPE3=BP(I3,4)
      DPE=DPE1+DPE3
      DPX1=BP(I1,1)
      DPX3=BP(I3,1)
      DBEX=(DPX1+DPX3)/DPE
      DPY1=BP(I1,2)
      DPY3=BP(I3,2)
      DBEY=(DPY1+DPY3)/DPE
      DPZ1=BP(I1,3)
      DPZ3=BP(I3,3)
      DBEZ=(DPZ1+DPZ3)/DPE
      CALL AROBO2(0.0,0.0,-DBEX,-DBEY,-DBEZ,I1,I3)
 
C...Calculate rotation angles but no need for rotation yet
      PX=BP(I1,1)
      PY=BP(I1,2)
      PZ=BP(I1,3)
      PHI=ULANGL(PX,PY)
      THE=ULANGL(PZ,SQRT(PX**2+PY**2))
      CALL AROBO2(0.0,-PHI,0.0D0,0.0D0,0.0D0,I1,I3)
      CALL AROBO2(-THE,0.0,0.0D0,0.0D0,0.0D0,I1,I3)
      IF(IEX(I1).GT.0) BP(I1,5)=0.0
      IF(IEX(I3).GT.0) BP(I3,5)=0.0
      BE=BP(I1,4)+BP(I3,4)
      BP(I1,4)=0.5*(BE**2+BP(I1,5)**2-BP(I3,5)**2)/BE
      BP(I3,4)=BE-BP(I1,4)
      BP(I1,3)=SQRT(BP(I1,4)**2-BP(I1,5)**2)
      BP(I3,3)=-BP(I1,3)
      BP(I1,2)=0.0
      BP(I3,2)=0.0
      BP(I1,1)=0.0
      BP(I3,1)=0.0
 
      CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,I1,I3)
 
      RETURN
 
C**** END OF AREXMA ****************************************************
      END
