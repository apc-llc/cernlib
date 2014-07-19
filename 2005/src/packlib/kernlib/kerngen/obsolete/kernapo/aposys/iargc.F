*
* $Id: iargc.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: iargc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      FUNCTION IARGC ()
C
C CERN PROGLIB#         IARGC           .VERSION KERNAPO  1.20  900328
C ORIG. 22/03/90  JZ
C
C-    APOLLO inplementation of UNIX function

      INTEGER*2    NARGS


      CALL PGM_$GET_ARGS (NARGS,LPOIARG)
      IARGC = NARGS - 1
      RETURN
      END
#ifdef CERNLIB_CCGEN_IARGC
#undef CERNLIB_CCGEN_IARGC
#endif
#ifdef CERNLIB_TCGEN_IARGC
#undef CERNLIB_TCGEN_IARGC
#endif
#endif
