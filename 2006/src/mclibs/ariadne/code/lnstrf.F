C***********************************************************************

      SUBROUTINE LNSTRF(X,XQ2,XPQ)

C...ariadne dummy routine LNSTRF

C...Produce an error message if Lepto structure functions has not
C...been linked.

#include "arimpl.f"

      REAL XPQ(-6:6)


      CALL ARERRM('LNSTRF',24,0)

      RETURN

C**** END OF LNSTRF ****************************************************
      END
