*
* $Id: zslistm.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslistm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_MMAP)
      SUBROUTINE ZSLISTM(IGOFF)
*.==========>
*.           To list  histograms in shared memory IPAWD (control routine)
*..=========> ( R.Brun)
#include "cspack/hcbook.inc"
*
      CALL ZSLISM(LQ(IGOFF+1))
      END
#endif
