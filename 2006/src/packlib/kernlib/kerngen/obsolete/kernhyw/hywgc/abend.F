*
* $Id: abend.F,v 1.1.1.1 1996/02/15 17:52:48 mclareni Exp $
*
* $Log: abend.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:48  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ABEND
C
C CERN PROGLIB#         ABEND           .VERSION KERNHYW  1.07  870624
C ORIG. 22/06/87 JZ
C     Provisional routine for abnormal job termination
C
      STOP 77
      END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif
*      ==================================================
#include "qcardl.inc"
