*
* $Id: memory.F,v 1.1.1.1 1996/04/01 15:03:18 mclareni Exp $
*
* $Log: memory.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:18  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if (defined(CERNLIB_F4))&&(!defined(CERNLIB_NOSBE))
      SUBROUTINE MEMORY(N,NTOT,IERR)
      N2 = N
      NTOT = INCMEM(N2)
      IERR = N2-N
      RETURN
      END
#endif
