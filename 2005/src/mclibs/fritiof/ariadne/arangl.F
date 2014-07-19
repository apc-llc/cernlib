*
* $Id: arangl.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: arangl.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arangl.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      REAL FUNCTION ARANGL(I1,I2)
 
C...ARiadne function ANGLe
 
C...Returns the angle between paron I1 and I2
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
 
      ARANGL=BP(I1,1)*BP(I2,1)+BP(I1,2)*BP(I2,2)+BP(I1,3)*BP(I2,3)
      BP1=SQRT(BP(I1,1)**2+BP(I1,2)**2+BP(I1,3)**2)
      BP2=SQRT(BP(I2,1)**2+BP(I2,2)**2+BP(I2,3)**2)
      ARANGL=ACOS(ARANGL/(BP1*BP2))
 
      RETURN
 
C**** END OF ARANGL ****************************************************
      END
