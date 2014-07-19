*
* $Id: zjumpft.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zjumpft.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_NORD)
      SUBROUTINE ZJUMP (NAME,P1,P2,P3,P4)
      DIMENSION P1(*),P2(*),P3(*),P4(*)
      CALL NAME (P1,P2,P3,P4)
      END
#endif
