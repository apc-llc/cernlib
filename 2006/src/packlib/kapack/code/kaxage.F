*
* $Id: kaxage.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxage.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
#if !defined(CERNLIB_IBM)||defined(CERNLIB_QMIBMXA)
      SUBROUTINE KAXAGE(IWAIT)
C
C Routine to wait for given time period IHUNS - in 1/100sec units
C
      CALL SLEEPF(IWAIT/100)
*
      END
#endif
