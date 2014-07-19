*
* $Id: iargv.F,v 1.1.1.1 1996/02/15 17:52:26 mclareni Exp $
*
* $Log: iargv.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IARGV (INARG,ARGV)
C
C CERN PROGLIB# Z264    IARGV           .VERSION KERNCVX  1.09  910815
C ORIG. 01/01/89  AGA+MAV, Convex
C
      CHARACTER    ARGV*(*)
C!    INTEGER*4    JARG

      JARG = INARG
      CALL GETARG (JARG,ARGV)
      IARGV = LNBLNK (ARGV)
      RETURN
      END
#ifdef CERNLIB_TCGEN_IARGV
#undef CERNLIB_TCGEN_IARGV
#endif
