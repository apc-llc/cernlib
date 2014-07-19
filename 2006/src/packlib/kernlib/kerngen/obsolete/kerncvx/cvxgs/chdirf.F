*
* $Id: chdirf.F,v 1.1.1.1 1996/02/15 17:52:23 mclareni Exp $
*
* $Log: chdirf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION CHDIRF (NAME)
C
C CERN PROGLIB#         CHDIRF          .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to chdir
C
      CHARACTER    NAME*(*)
      INTEGER      CHDIR

      N = LNBLNK (NAME)
      CHDIRF = CHDIR (NAME(1:N))
      RETURN
      END
#ifdef CERNLIB_CCGEN_CHDIRF
#undef CERNLIB_CCGEN_CHDIRF
#endif
#ifdef CERNLIB_CCGEN_CHDIRI
#undef CERNLIB_CCGEN_CHDIRI
#endif
