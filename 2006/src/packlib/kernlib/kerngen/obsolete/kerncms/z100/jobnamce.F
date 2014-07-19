*
* $Id: jobnamce.F,v 1.1.1.1 1996/02/15 17:51:49 mclareni Exp $
*
* $Log: jobnamce.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:49  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if (defined(CERNLIB_CERN))&&(defined(CERNLIB_NEVER))
      SUBROUTINE JOBNAM(NAME)
C
C CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCMS  1.02  890308
C
C  Version for (Hep)VM with SLAC Batch
C
C  Function:  Returns SLAC Batch Job identifier if in batch
C             or 'INTERACT' if interactive.
C
C  This version of JOBNAM merely provides a convenient interface to
C  the JOB$VM routine.
C
      CHARACTER*8 INFO(16),NAME
C
      CALL JOB$VM(INFO)
      NAME = INFO(5)
C
      RETURN
      END
#ifdef CERNLIB_TCGEN_JOBNAM
#undef CERNLIB_TCGEN_JOBNAM
#endif
#endif
