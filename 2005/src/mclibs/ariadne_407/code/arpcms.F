*
* $Id: arpcms.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arpcms.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpcms.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      REAL FUNCTION ARPCMS(S,SM1,SM2)
 
C...ARiadne function get Positive ligth-cone component in CMS
 
C...Returns the positive light-cone  component of a particle momentum
C...when placed in the cms system of itself and an other particle given
C...the two particle masses SM1 and SM2 and the total energy squared S
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      ARPCMS=-1
      A1=0.25*(S-(SM1+SM2)**2)
      IF (A1.LT.0.0) RETURN
      A2=(S-(SM1-SM2)**2)/S
      ARPCMS=SQRT(A1*A2)+SQRT(A1*A2+SM1**2)
 
      RETURN
 
C**** END OF ARPCMS ****************************************************
      END
