*
* $Id: gmtimef.F,v 1.1.1.1 1996/02/15 17:52:26 mclareni Exp $
*
* $Log: gmtimef.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GMTIMEF (CLOCK, ITIMEV)
C
C CERN PROGLIB#         GMTIMEF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to gmtime
C
C-        CLOCK   encoded time (returned by, e.g. STATF)
C-        ITIMEV  decoded time

      INTEGER      ITIMEV(9)
      INTEGER      CLOCK

      CALL GMTIME (CLOCK,ITIMEV)

      RETURN
      END
#ifdef CERNLIB_CCGEN_GMTIMEF
#undef CERNLIB_CCGEN_GMTIMEF
#endif
