*
* $Id: xzinit.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzinit.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
#include "cspack/czunit.inc"
*
*     Set parameters for XZ package: Log unit, log level, I/O units
*
      IXPRNT = IPRINT
      IDEBXZ = IDEBUG
      LUNXZI = LUNI
      LUNXZO = LUNO
      END
 
