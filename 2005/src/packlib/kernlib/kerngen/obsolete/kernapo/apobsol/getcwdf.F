*
* $Id: getcwdf.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: getcwdf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE GETCWDF (CHNAME,NNAME)
C
C CERN PROGLIB#         GETCWDF         .VERSION KERNAPO  1.21  900731
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
      NNAME = LENA
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETCWDF
#undef CERNLIB_CCGEN_GETCWDF
#endif
#ifdef CERNLIB_TCGEN_GETCWDF
#undef CERNLIB_TCGEN_GETCWDF
#endif
