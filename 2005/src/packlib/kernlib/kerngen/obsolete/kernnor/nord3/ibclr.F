*
* $Id: ibclr.F,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: ibclr.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION IBCLR(I,N)
C
C CERN PROGLIB# M441    IBCLR           .VERSION KERNNOR  2.04  870524
C ORIG. 01/11/85  H.RENSHALL
C
C      NORD-500 VERSION OF ANSI/ISA FUNCTION IBCLR
C      N IS THE POSTION OF THE BIT TO BE CLEARED
C
      J= I
      CALL CLBIT(J,N)
      IBCLR= J
      END
#ifdef CERNLIB_TCGEN_IBCLR
#undef CERNLIB_TCGEN_IBCLR
#endif
