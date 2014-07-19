*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNNOR  2.07  890912
C MOD.  27/07/89  add /SLATE/, JZ
C
      COMMON /SLATE/ MM(20), M(20)
 
      CALL CLOCK (M)
 
      DO 24  J=1,6
   24 MM(J) = M(8-J)
 
      M(7) = MOD(M(7),100)
      ND = 10000*M(7)+100*M(6)+M(5)
      NT = 100*M(4)+M(3)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
