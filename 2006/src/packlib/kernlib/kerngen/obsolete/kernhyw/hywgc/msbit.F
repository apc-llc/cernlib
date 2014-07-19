*
* $Id: msbit.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: msbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MSBIT (MZ,IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT           .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
 
      MSBIT = IOR (IAND (IZW, NOT(ILS(1,IZP-1)) )
     +,            ILS(IAND(MZ,1),IZP-1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
*      ==================================================
#include "qcardl.inc"
