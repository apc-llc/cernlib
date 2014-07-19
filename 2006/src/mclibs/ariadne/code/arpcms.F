C***********************************************************************
C $Id: arpcms.F,v 1.2 1996/04/10 12:33:27 mclareni Exp $

      REAL FUNCTION ARPCMS(S,SM1,SM2)

C...ARiadne function get Positive ligth-cone component in CMS

C...Returns the positive light-cone  component of a particle momentum
C...when placed in the cms system of itself and an other particle given
C...the two particle masses SM1 and SM2 and the total energy squared S

#include "arimpl.f"

      ARPCMS=-1
      A1=0.25*(S-(SM1+SM2)**2)
      IF (A1.LT.0.0) RETURN
      A2=(S-(SM1-SM2)**2)/S
      ARPCMS=SQRT(A1*A2)+SQRT(A1*A2+SM1**2)

      RETURN

C**** END OF ARPCMS ****************************************************
      END
