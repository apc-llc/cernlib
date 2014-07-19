*
* $Id: gzisam.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisam.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISAM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     get pointer to first ISAM
C-   Returned value  : pointer
C-
C-   D. Hedin 15-oct-90
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISAM
#include "zebcom.inc"
#include "izisam.inc"
C----------------------------------------------------------------------
C
      IF(LISAE.GT.0) GZISAM=LQ(LISAE-IZISAM)
  999 RETURN
      END
