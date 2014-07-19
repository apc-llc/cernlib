*
* $Id: armass.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: armass.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: armass.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      REAL FUNCTION ARMASS(N,I)
 
C...ARiadne function invariant MASS of partons
 
C...Returns the total invariant mass^2 of N partons
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
      DIMENSION I(N),DPS(4)
 
 
      DO 100 IK=1,4
        DPS(IK)=0.0D0
        DO 200 IJ=1,N
          DPS(IK)=DPS(IK)+BP(I(IJ),IK)
 200    CONTINUE
 100  CONTINUE
 
      DMASS=DPS(4)**2-DPS(3)**2-DPS(2)**2-DPS(1)**2
      ARMASS=MAX(DMASS,0.0D0)
 
      RETURN
 
C**** END OF ARMASS ****************************************************
      END
