*
* $Id: ibclr.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: ibclr.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION IBCLR(I,N)
C
C CERN PROGLIB# M441    IBCLR           .VERSION KERNCRY  1.06  870527
C ORIG. 17/11/86  CNAF, BOLOGNA
C
C      CRAY VERSION OF ANSI/ISA FUNCTION IBCLR
C      N IS THE POSTION OF THE BIT TO BE CLEARED
C
      IBCLR = AND(I, COMPL(SHIFTL(1,N)))
      END
#ifdef CERNLIB_TCGEN_IBCLR
#undef CERNLIB_TCGEN_IBCLR
#endif
