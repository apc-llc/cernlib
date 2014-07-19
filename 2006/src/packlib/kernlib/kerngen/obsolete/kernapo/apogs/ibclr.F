*
* $Id: ibclr.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: ibclr.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION IBCLR (I,N)
C
C CERN PROGLIB# M441    IBCLR           .VERSION KERNAPO  1.10  870524
C ORIG. 01/10/85  T.LINDELOF
C
C      APOLLO VERSION OF ANSI/ISA FUNCTION IBCLR
C      N IS THE POSTION OF THE BIT TO BE CLEARED
C
      IBCLR = AND(I, NOT(LSHFT(1, N)))
      END
#ifdef CERNLIB_TCGEN_IBCLR
#undef CERNLIB_TCGEN_IBCLR
#endif
