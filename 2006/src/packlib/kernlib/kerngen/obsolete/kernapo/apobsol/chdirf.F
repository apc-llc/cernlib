*
* $Id: chdirf.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: chdirf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      INTEGER FUNCTION CHDIRF (CHNAME)
C
C CERN PROGLIB#         CHDIRF          .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHNAME*(*)
      INTEGER*2    LENA

      ISTAT = -1
      LENA  = ICFIND (' ', CHNAME,1,LEN(CHNAME)) - 1
      IF (LENA.GT.0)  CALL NAME_$SET_WDIR (CHNAME,LENA,ISTAT)
      CHDIRF = ISTAT
      RETURN
      END
#ifdef CERNLIB_CCGEN_CHDIRF
#undef CERNLIB_CCGEN_CHDIRF
#endif
#ifdef CERNLIB_TCGEN_CHDIRF
#undef CERNLIB_TCGEN_CHDIRF
#endif
