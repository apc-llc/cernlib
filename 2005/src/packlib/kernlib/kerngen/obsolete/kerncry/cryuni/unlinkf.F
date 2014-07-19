*
* $Id: unlinkf.F,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
*
* $Log: unlinkf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      INTEGER FUNCTION UNLINKF (NAME)
C
C CERN PROGLIB# Z265    UNLINKF         .VERSION KERNCRY  1.16  911111
C ORIG. 17/10/91, JZ
C Fortran interface routine to unlink
C
      CHARACTER    NAME*(*)
      INTEGER      UNLINK

      LGNAME  = LNBLNK (NAME)
      UNLINKF = UNLINK (NAME(1:LGNAME))
      RETURN
      END
#ifdef CERNLIB_CCGEN_UNLINKF
#undef CERNLIB_CCGEN_UNLINKF
#endif
#ifdef CERNLIB_CCGEN_UNLINI
#undef CERNLIB_CCGEN_UNLINI
#endif
#ifdef CERNLIB_TCGEN_UNLINKF
#undef CERNLIB_TCGEN_UNLINKF
#endif
