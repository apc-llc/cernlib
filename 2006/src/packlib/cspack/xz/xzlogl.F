*
* $Id: xzlogl.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzlogl.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLOGL(ILOG)
#include "cspack/czunit.inc"
      IDEBXZ = ILOG
      IF(IDEBXZ.GE.-2) PRINT *,'XZLOGL. Setting log level to ',IDEBXZ
      END
