C***********************************************************************
C $Id: arnofl.F,v 1.2 1996/04/10 12:33:24 mclareni Exp $

      REAL FUNCTION ARNOFL(W,MNOFL)

C...ARiadne function Number Of FLavours 

C...Returns the number of flavourspossible at energy W

#include "ardat2.f"


      ARNOFL=0.0
      DO 100 I=1,MNOFL
        IF (W.LT.2.0*PQMAS(I)) RETURN
        ARNOFL=REAL(I)
 100  CONTINUE

      RETURN

C**** END OF ARNOFL ****************************************************
      END
