*
* $Id: dranf.F,v 1.1.1.1 1996/02/15 17:48:18 mclareni Exp $
*
* $Log: dranf.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:18  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          DOUBLE PRECISION FUNCTION DRANF()
          DOUBLE PRECISION VALUE
          REAL             CONST, TEMP(2)
          EQUIVALENCE      (VALUE,TEMP(1))
#if defined(CERNLIB_NUMCD)
          DATA      CONST     /  O"1640 4000 0000 0000 0000"  /
#endif
#if defined(CERNLIB_NUMCR)
          DATA      CONST     /  7777 7777 7777 7777B  /
#endif
          TEMP(1)  =  RANF()
#if defined(CERNLIB_NUMCD)
          TEMP(2)  =  TEMP(1)*CONST
#endif
#if defined(CERNLIB_NUMCR)
          TEMP(2)  =  TEMP(1) .AND. CONST
#endif
          DRANF    =  VALUE
          RETURN
          END
