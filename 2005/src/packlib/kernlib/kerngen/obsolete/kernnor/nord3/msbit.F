*
* $Id: msbit.F,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: msbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION MSBIT (MZ,IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT           .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
 
      MSBIT = IOR (IAND (IZW, NOT(ISHFT(1,IZP-1)) )
     +,            ISHFT(IAND(MZ,1),IZP-1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
