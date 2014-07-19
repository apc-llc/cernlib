*
* $Id: arrobo.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arrobo.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arrobo.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARROBO(THE,PHI,DBEX,DBEY,DBEZ,NI,I)
 
C...ARiadne subroutine ROtate and BOost
 
C...Rotates and boost NI particles in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      DIMENSION I(NI),BR(3,3),BV(3),DP(4)
 
 
      IF(THE**2+PHI**2.GT.1.0E-20) THEN
 
C...Rotate (typically from z axis to direction theta,phi)
 
        SP=SIN(PHI)
        CP=COS(PHI)
        ST=SIN(THE)
        CT=COS(THE)
 
        BR(1,1)=CT*CP
        BR(1,2)=-SP
        BR(1,3)=ST*CP
        BR(2,1)=CT*SP
        BR(2,2)=CP
        BR(2,3)=ST*SP
        BR(3,1)=-ST
        BR(3,2)=0.0
        BR(3,3)=CT
 
        DO 100 IJ=1,NI
          DO 110 J=1,3
            BV(J)=BP(I(IJ),J)
 110      CONTINUE
          DO 120 J=1,3
            BP(I(IJ),J)=BR(J,1)*BV(1)+BR(J,2)*BV(2)+BR(J,3)*BV(3)
 120      CONTINUE
 100    CONTINUE
 
      ENDIF
 
      DBTOT2=DBEX**2+DBEY**2+DBEZ**2
      IF(DBTOT2.GT.1.0D-20) THEN
        IF(DBTOT2.GE.1.0D0) CALL ARERRM('ARROBO',14,0)
        DGA=1.0D0/DSQRT(1.0D0-DBTOT2)
 
        DO 200 IJ=1,NI
          DO 210 J=1,4
            DP(J)=BP(I(IJ),J)
 210      CONTINUE
          DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
          DGABEP=DGA*(DGA*DBEP/(1.0D0+DGA)+DP(4))
 
          BP(I(IJ),1)=DP(1)+DGABEP*DBEX
          BP(I(IJ),2)=DP(2)+DGABEP*DBEY
          BP(I(IJ),3)=DP(3)+DGABEP*DBEZ
          BP(I(IJ),4)=DGA*(DP(4)+DBEP)
 
 200    CONTINUE
 
      ENDIF
 
      RETURN
 
C**** END OF ARROBO ****************************************************
      END
