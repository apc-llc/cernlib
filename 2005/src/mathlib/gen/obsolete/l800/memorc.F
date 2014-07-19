*
* $Id: memorc.F,v 1.1.1.1 1996/04/01 15:03:19 mclareni Exp $
*
* $Log: memorc.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:19  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_NOSBE)||!defined(CERNLIB_F4)
      SUBROUTINE MEMORC (N,NTOT,IERR)
      DATA NDABA/0/
C
      IF (NDABA.EQ.0) NDABA=NXDABA (DUM)
      NTOT=NDABA
      IERR=0
      IF (NDABA.LT.0) IERR=-1
      RETURN
      END
#endif
