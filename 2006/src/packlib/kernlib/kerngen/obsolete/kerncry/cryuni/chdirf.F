*
* $Id: chdirf.F,v 1.1.1.1 1996/02/15 17:52:42 mclareni Exp $
*
* $Log: chdirf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:42  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      INTEGER FUNCTION CHDIRF (NAME)
C
C CERN PROGLIB# Z265    CHDIRF          .VERSION KERNCRY  1.16  911111
C ORIG. 17/10/91, JZ
C Fortran interface routine to chdir
C
      CHARACTER    NAME*(*)
      INTEGER      CHDIR

      CHDIRF = CHDIR (NAME)
      RETURN
      END
#ifdef CERNLIB_CCGEN_CHDIRF
#undef CERNLIB_CCGEN_CHDIRF
#endif
#ifdef CERNLIB_CCGEN_CHDIRI
#undef CERNLIB_CCGEN_CHDIRI
#endif
