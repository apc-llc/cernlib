*
* $Id: cfrascun.F,v 1.1.1.1 1996/03/08 15:21:49 mclareni Exp $
*
* $Log: cfrascun.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:49  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_CRAY)||defined(CERNLIB_NORD)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_VAX)||defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      SUBROUTINE CFRASC(IARR,NW)
C   DUMMY FOR ASCII MACHINES
      RETURN
      END
#endif
