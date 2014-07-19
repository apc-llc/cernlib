C***********************************************************************

      SUBROUTINE PYSTFU(KF,X,XQ2,XPQ)

C...ariadne dummy routine PYSTFU

C...Produce an error message if Lepto structure functions has not
C...been linked.

#include "arimpl.f"

      REAL XPQ(-25:25)


      CALL ARERRM('PYSTFU',24,0)

      RETURN

C**** END OF PYSTFU ****************************************************
      END
