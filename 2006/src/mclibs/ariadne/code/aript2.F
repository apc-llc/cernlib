C***********************************************************************
C $Id: aript2.F,v 1.2 1996/04/10 12:33:20 mclareni Exp $

      REAL FUNCTION ARIPT2(I1,I2,I3)

C...ARiadne function Invariant PT2

C...Returns the invariant p_t^2 of three partons

#include "arimpl.f"
#include "arpart.f"


      ARIPT2=(ARMAS2(I1,I2)-(BP(I1,5)+BP(I2,5))**2)*
     $       (ARMAS2(I2,I3)-(BP(I2,5)+BP(I3,5))**2)/
     $        ARMAS3(I1,I2,I3)

      RETURN

C**** END OF ARIPT2 ****************************************************
      END
