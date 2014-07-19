*
* $Id: cbyt.F,v 1.1.1.1 1996/02/15 17:52:48 mclareni Exp $
*
* $Log: cbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:48  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CBYT (IWS,JPS,IWT,JPT,NBITS)
C
C CERN PROGLIB# M421    CBYT            .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      JPTH = 37 - JPT - NBITS
      FLD(JPTH,NBITS,IWT) = FLD(37-JPS-NBITS,NBITS,IWS)
      RETURN
      END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
*      ==================================================
#include "qcardl.inc"
