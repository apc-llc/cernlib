*
* $Id: msbit.F,v 1.1.1.1 1996/02/15 17:51:04 mclareni Exp $
*
* $Log: msbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:04  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION MSBIT (MZ,IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT           .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C

      MSBIT = OR (AND (IZW, NOT(LSHFT(1,IZP-1)) )
     +          , LSHFT(AND(MZ,1),IZP-1) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
