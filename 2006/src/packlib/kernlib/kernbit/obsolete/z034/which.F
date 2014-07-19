*
* $Id: which.F,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: which.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kernbit/pilot.h"
#if !defined(CERNLIB_IBM)
      SUBROUTINE WHICH(ISIT,IVAL)
C         CERN PACKAGE Z034   MACHINE/SYSTEM IDENTIFICATION
#endif
      DATA IMAC / 4H    /
      ISIT = 0
      IVAL = IMAC
      END
