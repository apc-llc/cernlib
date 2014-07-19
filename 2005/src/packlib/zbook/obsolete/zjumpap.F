*
* $Id: zjumpap.F,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: zjumpap.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_UNIX))
      SUBROUTINE ZJUMP (NAME,P1,P2,P3,P4)
      DIMENSION P1(*),P2(*),P3(*),P4(*)
      INTEGER*4 IPOINT
      POINTER /IPOINT/ M
C
      IPOINT=NAME
      CALL ZJUMP2(M,P1,P2,P3,P4)
      END
      SUBROUTINE ZJUMP2(SUBR,P1,P2,P3,P4)
      EXTERNAL SUBR
      DIMENSION P1(*),P2(*),P3(*),P4(*)
C
      CALL SUBR(P1,P2,P3,P4)
      END
#endif
