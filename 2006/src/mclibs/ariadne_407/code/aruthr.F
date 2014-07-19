*
* $Id: aruthr.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
*
* $Log: aruthr.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:07  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aruthr.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
 
      REAL FUNCTION ARUTHR(ID,JRAD,I1,I3,IN1,IN2)
 
C...ARiadne dummy routine User THRow emission
 
C...Enables a user to check each emission and trow it away if it
C...doesn't meet some criteria
 
C...The arguments are the radiating dipole ID, the original partons
C...I1 and I3 of the dipole, the first IN1 and last IN2 radiated
C...parton and the type of emission JRAD. The routine should return a
C...negative number if the emission doesn't meet the specified
C...conditions, positive otherwise.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
 
      ARUTHR=1.0
 
      RETURN
 
C**** END OF ARUTHR ****************************************************
      END
