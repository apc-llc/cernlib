*
* $Id: jbytet.F,v 1.1.1.1 1996/02/15 17:52:50 mclareni Exp $
*
* $Log: jbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:50  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JBYTET (MASK,IW,JPOS,NBITS)
C
C CERN PROGLIB# M421    JBYTET          .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      JBYTET = AND (MASK,FLD(37-JPOS-NBITS,NBITS,IW))
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
*      ==================================================
#include "qcardl.inc"
