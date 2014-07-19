*
* $Id: jobnam.F,v 1.1.1.1 1996/02/15 17:51:49 mclareni Exp $
*
* $Log: jobnam.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:49  mclareni
* Kernlib
*
*
*IF=-CERN.
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE JOBNAM(NAME)
C
C CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCMS  1.06  910721
C
C  Returns Batch Job identifier or INTERACT if interactive
C  Usable with SLAC-Batch and VMBatch
C
      CHARACTER*8 NAME,JOBINFO
C
      NAME = JOBINFO('JOBID')
C
      RETURN
      END
#ifdef CERNLIB_TCGEN_JOBNAM
#undef CERNLIB_TCGEN_JOBNAM
#endif
