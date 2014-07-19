*
* $Id: job_vm.F,v 1.1.1.1 1996/02/15 17:51:56 mclareni Exp $
*
* $Log: job_vm.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:56  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if !defined(CERNLIB_CERN)
      LOGICAL FUNCTION JOB$VM(INFO)
C
C Dummy Batch interface routine for non-HepVM sites.
C See Patch SYCERNV for original assembler and detailed description
C
      CHARACTER*128 INFO,DEFAULT
      CHARACTER*80 SPACES
      REAL NUMBRS(32)
      EQUIVALENCE (SPACES,DEFAULT) , (NUMBRS(1),DEFAULT)
C
C Blank out undecidable fields
C
      DATA SPACES/' '/
C
C Default job time limit
C
      DATA NUMBRS(21)/ 9999.0 /
      DATA NUMBRS(22)/    0.0 /
C
C Default CPU normalisation
C
      DATA NUMBRS(23)/    1.0 /
      DATA NUMBRS(24)/    0.0 /
C
C Don't know whether we're 370 or XA
C
      DATA NUMBRS(23)/    0.0 /
      DATA NUMBRS(24)/    0.0 /
C
C Send back all the default details
C
      INFO = DEFAULT
C
C And we're not in SLAC Batch either...
C
      JOB$VM = .FALSE.
C
      RETURN
      END
#endif
