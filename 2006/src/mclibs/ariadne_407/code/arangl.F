*
* $Id: arangl.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arangl.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arangl.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      REAL FUNCTION ARANGL(I1,I2)
 
C...ARiadne function ANGLe
 
C...Returns the angle between parton I1 and I2
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
 
 
      D12=BP(I1,1)*BP(I2,1)+BP(I1,2)*BP(I2,2)+BP(I1,3)*BP(I2,3)
      DP1=SQRT(BP(I1,1)**2+BP(I1,2)**2+BP(I1,3)**2)
      DP2=SQRT(BP(I2,1)**2+BP(I2,2)**2+BP(I2,3)**2)
      ARANGL=ACOS(MAX(-1.0D0,MIN(1.0D0,D12/(DP1*DP2))))
 
      RETURN
 
C**** END OF ARANGL ****************************************************
      END
