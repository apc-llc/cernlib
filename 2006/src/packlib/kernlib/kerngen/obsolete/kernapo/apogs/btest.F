*
* $Id: btest.F,v 1.1.1.1 1996/02/15 17:51:01 mclareni Exp $
*
* $Log: btest.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:01  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      LOGICAL FUNCTION BTEST (I,N)
C
C CERN PROGLIB# M441    BTEST           .VERSION KERNAPO  1.10  870524
C ORIG. 01/10/85  T.LINDELOF, corrected 19/05/87
C
C      APOLLO VERSION OF ANSI/ISA FUNCTION BTEST
C      N IS THE BIT TO BE TESTED
C
      BTEST = AND(I, LSHFT(1, N)) .NE. 0
      END
#ifdef CERNLIB_TCGEN_BTEST
#undef CERNLIB_TCGEN_BTEST
#endif
