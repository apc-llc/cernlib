*
* $Id: statf.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: statf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION STATF (NAME, INFO)
C
C CERN PROGLIB#         STATF           .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to stat
C
C        NAME   the name of the file of which information is required
C     INFO(12)  buffer returning the information about "file"
C
C        A succesfull call returns 0. A failed call returns -1.
C
      CHARACTER*(*)  NAME
      INTEGER      STAT, INFO(12), INST(16)

      STATF = STAT (NAME,INST)

      JPUT = 1
      DO 24  JTAK=1,13
      IF (JTAK.EQ.7)         GO TO 24
      INFO(JPUT) = INST(JTAK)
      JPUT = JPUT + 1
   24 CONTINUE

      RETURN
      END
#ifdef CERNLIB_CCGEN_STATF
#undef CERNLIB_CCGEN_STATF
#endif
#ifdef CERNLIB_CCGEN_STATI
#undef CERNLIB_CCGEN_STATI
#endif
