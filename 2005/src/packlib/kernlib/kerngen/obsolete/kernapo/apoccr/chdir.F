*
* $Id: chdir.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: chdir.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      INTEGER FUNCTION CHDIR (CHNAME)
C
C CERN PROGLIB#         CHDIR           .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHNAME*(*)
      INTEGER*2    LENA

      LENA = LNBLNK (CHNAME)
      CALL NAME_$SET_WDIR (CHNAME,LENA,ISTAT)
      CHDIR = ISTAT
      RETURN
      END
#ifdef CERNLIB_CCGEN_CHDIR
#undef CERNLIB_CCGEN_CHDIR
#endif
#ifdef CERNLIB_TCGEN_CHDIR
#undef CERNLIB_TCGEN_CHDIR
#endif
