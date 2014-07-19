*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:52:48 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:48  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIME (ID,IT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      DIMENSION    M(8)
      CHARACTER*8  DCH
 
      CALL DATIM (DCH,T)
      CALL UBLOW (DCH,M,8)
 
      MY = IUFORW (M,7,8)
      MM = IUFORW (M,1,2)
      MD = IUFORW (M,4,5)
      ID = 100*(100*MY+MM)+MD
      IT = 100.*T
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
*      ==================================================
#include "qcardl.inc"
