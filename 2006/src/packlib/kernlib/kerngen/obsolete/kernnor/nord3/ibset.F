*
* $Id: ibset.F,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: ibset.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION IBSET(I,N)
C
C CERN PROGLIB# M441    IBSET           .VERSION KERNNOR  2.04  870524
C ORIG. 01/11/85  H.RENSHALL
C
C      NORD-500 VERSION OF ANSI/ISA FUNCTION IBSET
C      N IS THE POSITION OF THE BIT TO BE SET
C
      J= I
      CALL STBIT(J,N)
      IBSET= J
      END
#ifdef CERNLIB_TCGEN_IBSET
#undef CERNLIB_TCGEN_IBSET
#endif
