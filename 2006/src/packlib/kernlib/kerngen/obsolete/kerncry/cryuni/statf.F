*
* $Id: statf.F,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
*
* $Log: statf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      INTEGER FUNCTION STATF (NAME, INFO)
C
C CERN PROGLIB# Z265    STATF           .VERSION KERNCRY  1.16  911111
C ORIG. 17/10/91, JZ
C Fortran interface routine to stat
C
C        NAME   the name of the file of which information is required
C     INFO(12)  buffer returning the information about "file"
C
C        A succesfull call returns 0. A failed call returns -1.
C
      CHARACTER*(*)  NAME
      INTEGER        INFO(12), STAT

      STATF = STAT (NAME, INFO)
      RETURN
      END
#ifdef CERNLIB_CCGEN_STATF
#undef CERNLIB_CCGEN_STATF
#endif
#ifdef CERNLIB_CCGEN_STATI
#undef CERNLIB_CCGEN_STATI
#endif
