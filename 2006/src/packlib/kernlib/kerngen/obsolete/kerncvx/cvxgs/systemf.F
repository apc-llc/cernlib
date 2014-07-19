*
* $Id: systemf.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: systemf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION SYSTEMF (NAME)
C
C CERN PROGLIB#         SYSTEMF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to system
C
      CHARACTER    NAME*(*)
      INTEGER      SYSTEM

      SYSTEMF = SYSTEM (NAME)
      RETURN
      END
#ifdef CERNLIB_CCGEN_SYSTEMF
#undef CERNLIB_CCGEN_SYSTEMF
#endif
#ifdef CERNLIB_CCGEN_SYSTEI
#undef CERNLIB_CCGEN_SYSTEI
#endif
