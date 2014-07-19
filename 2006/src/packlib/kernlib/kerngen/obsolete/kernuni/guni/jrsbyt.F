*
* $Id: jrsbyt.F,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: jrsbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      COMPILER(FLD=ABS)
C                                  LAST MODIF.  21/03/78
C                                  FROM COPENHAGEN 3/35/72
 
      FUNCTION JRSBYT(IVAL,X,JTH,NBITS)
 
      JRSBYT=FLD(37-JTH-NBITS,NBITS,X)
      FLD(37-JTH-NBITS,NBITS,X)=IVAL
      RETURN
      END
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
