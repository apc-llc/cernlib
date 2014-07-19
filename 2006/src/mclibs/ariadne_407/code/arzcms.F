*
* $Id: arzcms.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arzcms.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arzcms.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      REAL FUNCTION ARZCMS(S,SM1,SM2)
 
C...ARiadne function get Z component in CMS
 
C...Returns the z component of a particle momentum when placed in the
C...cms system of itself and an other particle given the two particle
C...masses SM1 and SM2 and the total energy squared S
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      ARZCMS=-1
      AA=(S-(SM1+SM2)**2)
      IF (AA.LT.0.0) RETURN
      ARZCMS=0.5*SQRT(AA*(S-(SM1-SM2)**2)/S)
 
      RETURN
 
C**** END OF ARZCMS ****************************************************
      END
