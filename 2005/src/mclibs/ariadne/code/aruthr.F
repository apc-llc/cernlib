C***********************************************************************
C $Id: aruthr.F,v 1.2 1996/04/10 12:33:44 mclareni Exp $

      REAL FUNCTION ARUTHR(ID,JRAD,I1,I3,IN1,IN2)

C...ARiadne dummy routine User THRow emission

C...Enables a user to check each emission and trow it away if it
C...doesn't meet some criteria

C...The arguments are the radiating dipole ID, the original partons
C...I1 and I3 of the dipole, the first IN1 and last IN2 radiated
C...parton and the type of emission JRAD. The routine should return a
C...negative number if the emission doesn't meet the specified
C...conditions, positive otherwise.

#include "arimpl.f"


      ARUTHR=1.0

      RETURN

C**** END OF ARUTHR ****************************************************
      END
