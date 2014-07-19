*
* $Id: sbit0.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: sbit0.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE SBIT0 (IZW,IZP)
C
C CERN PROGLIB# M421    SBIT0           .VERSION KERNCRY  1.16  911111
C ORG. 24/10/91, JZ
C
      IZW =  IZW .AND. SHIFT(MASK(65),IZP)
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
