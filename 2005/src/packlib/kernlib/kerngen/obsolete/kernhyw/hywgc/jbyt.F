*
* $Id: jbyt.F,v 1.1.1.1 1996/02/15 17:52:50 mclareni Exp $
*
* $Log: jbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:50  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JBYT (IW,JPOS,NBITS)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      JBYT = FLD(37-JPOS-NBITS,NBITS,IW)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
*      ==================================================
#include "qcardl.inc"
