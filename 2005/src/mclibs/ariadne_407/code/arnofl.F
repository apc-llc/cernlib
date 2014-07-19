*
* $Id: arnofl.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: arnofl.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arnofl.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      REAL FUNCTION ARNOFL(W,MNOFL)
 
C...ARiadne function Number Of FLavours
 
C...Returns the number of flavourspossible at energy W
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
 
      ARNOFL=0.0
      DO 100 I=1,MNOFL
        IF (W.LT.2.0*PQMAS(I)) RETURN
        ARNOFL=REAL(I)
 100  CONTINUE
 
      RETURN
 
C**** END OF ARNOFL ****************************************************
      END
