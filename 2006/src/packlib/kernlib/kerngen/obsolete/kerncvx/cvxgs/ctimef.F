*
* $Id: ctimef.F,v 1.1.1.1 1996/02/15 17:52:23 mclareni Exp $
*
* $Log: ctimef.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CTIMEF (CLOCK, CHTIME)
C
C CERN PROGLIB#         CTIMEF          .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to ctime
C
C-        CLOCK   encoded time (returned by, e.g. STATF)
C-        CHTIME  decoded time string of length 24

      INTEGER      CLOCK
      CHARACTER    CHTIME*24, CTIME*24


      CHTIME = CTIME (CLOCK)
      RETURN
      END
#ifdef CERNLIB_CCGEN_CTIMEF
#undef CERNLIB_CCGEN_CTIMEF
#endif
