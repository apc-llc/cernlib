*
* $Id: ibset.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: ibset.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION IBSET(I,N)
C
C CERN PROGLIB# M441    IBSET           .VERSION KERNCRY  1.06  870527
C ORIG. 17/11/86  CNAF, BOLOGNA
C
C      CRAY VERSION OF ANSI/ISA FUNCTION IBSET
C      N IS THE POSITION OF THE BIT TO BE SET
C
      IBSET = OR(I, SHIFTL(1,N))
      END
#ifdef CERNLIB_TCGEN_IBSET
#undef CERNLIB_TCGEN_IBSET
#endif
