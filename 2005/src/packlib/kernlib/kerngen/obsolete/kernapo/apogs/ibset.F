*
* $Id: ibset.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: ibset.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION IBSET (I,N)
C
C CERN PROGLIB# M441    IBSET           .VERSION KERNAPO  1.10  870524
C ORIG. 01/10/85  T.LINDELOF
C
C      APOLLO VERSION OF ANSI/ISA FUNCTION IBSET
C      N IS THE POSITION OF THE BIT TO BE SET
C
      IBSET = OR(I, LSHFT(1, N))
      END
#ifdef CERNLIB_TCGEN_IBSET
#undef CERNLIB_TCGEN_IBSET
#endif
