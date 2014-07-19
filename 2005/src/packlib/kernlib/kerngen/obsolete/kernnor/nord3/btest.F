*
* $Id: btest.F,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: btest.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      LOGICAL FUNCTION BTEST(I,N)
C
C CERN PROGLIB# M441    BTEST           .VERSION KERNNOR  2.04  870524
C ORIG. 01/11/85  H.RENSHALL
C
C      NORD-500 VERSION OF ANSI/ISA FUNCTION BTEST
C      N IS THE BIT TO BE TESTED
C
      BTEST = .FALSE.
      IF (IBIT(I,N).NE.0) BTEST=.TRUE.
      END
#ifdef CERNLIB_TCGEN_BTEST
#undef CERNLIB_TCGEN_BTEST
#endif
