*
* $Id: arsmpt.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arsmpt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
      SUBROUTINE ARSMPT(I1,I2,I3)
 
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      P12=P(I1,4)*P(I2,4)-P(I1,3)*P(I2,3)-
     $    P(I1,2)*P(I2,2)-P(I1,1)*P(I2,1)
      P23=P(I3,4)*P(I2,4)-P(I3,3)*P(I2,3)-
     $    P(I3,2)*P(I2,2)-P(I3,1)*P(I2,1)
      P31=P(I1,4)*P(I3,4)-P(I1,3)*P(I3,3)-
     $    P(I1,2)*P(I3,2)-P(I1,1)*P(I3,1)
      PT2I=(V(I1,1)+V(I2,1)+P12)*(V(I2,1)+V(I3,1)+P23)/
     $     (V(I1,1)+V(I2,1)+V(I3,1)+P12+P23+P31)
 
      IF(PT2I.GE.V(I2,5)) RETURN
      V(I2,5)=PT2I
      K(I2,3)=I1
      K(I2,4)=I3
 
      RETURN
      END
