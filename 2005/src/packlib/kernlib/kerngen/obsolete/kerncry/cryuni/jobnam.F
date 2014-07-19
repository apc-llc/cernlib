*
* $Id: jobnam.F,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
*
* $Log: jobnam.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE JOBNAM (ID)
C
C CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCRY  1.10  890322
C ORIG. 20/12/88 FCA
C
      CHARACTER*8 ID, RNAME
      INTEGER GETENV
C
      IB = GETENV ('ENVIRONMENT'L, K, 1)
      IF (IB  .NE. 0) THEN
C         The ENVIRONMENT variable is defined, we are a batch job
          IRC = GETENV ('QSUB_REQNAME'L, RNAME, 1)
          I = INDEX (RNAME, '_')
          IF (I .EQ. 0) I = INDEX (RNAME, '.')
          IF (I .NE. 0) RNAME = RNAME(1:I-1)
      ELSE
C         We are an interactive process
          IRC = GETENV ('LOGNAME'L, RNAME, 1)
      ENDIF
      ID = RNAME
      END
#ifdef CERNLIB_TCGEN_JOBNAM
#undef CERNLIB_TCGEN_JOBNAM
#endif
