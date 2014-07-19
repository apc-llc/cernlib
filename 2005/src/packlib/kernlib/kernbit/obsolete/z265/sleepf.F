*
* $Id: sleepf.F,v 1.1.1.1 1996/02/15 17:47:44 mclareni Exp $
*
* $Log: sleepf.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kernbit/pilot.h"
#if defined(CERNLIB_IBMMVS)
      INTEGER FUNCTION SLEEPF(ISEC)
C
C CERN PROGLIB#    Z265 SLEEPF          .VERSION KERNIBM  X.XX  910308
C ORIG. 08/03/91, Harald Keller / DESY
C Fortran interface to SLEEP
C
      DATA CYCLE/26E-6/
      NCYCS=IFIX(1./CYCLE) * ISEC
      CALL DELAY(NCYCS)
      SLEEPF = 0
      RETURN
      END
#endif
