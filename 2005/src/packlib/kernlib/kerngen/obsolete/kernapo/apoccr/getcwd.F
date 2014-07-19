*
* $Id: getcwd.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: getcwd.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE GETCWD (CHNAME)
C
C CERN PROGLIB#         GETCWD          .VERSION KERNAPO  1.21  900731
C ORIG. 25/07/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHNAME*(*)
      INTEGER*2    LENA

      CALL NAME_$GET_WDIR (CHNAME,LENA,ISTAT)
      IF (ISTAT.NE.0)  THEN
          LENA = 0
        ELSE
          CALL CUTOL (CHNAME(1:LENA))
        ENDIF
      CHNAME(LENA+1:) = ' '
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETCWD
#undef CERNLIB_CCGEN_GETCWD
#endif
#ifdef CERNLIB_TCGEN_GETCWD
#undef CERNLIB_TCGEN_GETCWD
#endif
